{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use <$>" #-}

module Parser
  ( parseLaneProg,
  )
where

import Control.Monad (void)
import Raw (RExpr (..), RProg (..), RTLStmt (..), RType (..), TypedName (..), RBranch (..))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (LanguageDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser (..), TokenParser, makeTokenParser)
import Control.Monad.Identity (Identity)

laneLangDef :: LanguageDef ()
laneLangDef =
  LanguageDef
    { commentStart = "{-",
      commentEnd = "-}",
      commentLine = "--",
      nestedComments = True,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      reservedOpNames = laneReservedOps,
      reservedNames = laneReservedNames,
      caseSensitive = True
    }

lexer :: TokenParser ()
lexer = makeTokenParser laneLangDef

pParens = Text.Parsec.Token.parens lexer

pBraces = Text.Parsec.Token.braces lexer

pBrackets = Text.Parsec.Token.brackets lexer

pAngles = Text.Parsec.Token.angles lexer

pReserved = Text.Parsec.Token.reserved lexer

pSemiSep = Text.Parsec.Token.semiSep lexer

pReservedOp = Text.Parsec.Token.reservedOp lexer

pIdentifier = Text.Parsec.Token.identifier lexer

parseLaneProg :: String -> Either ParseError RProg
parseLaneProg = parse laneParser "(unknown)"

laneParser :: Parser RProg
laneParser = do
  prog <- pProg
  eof
  return prog

lineComment :: Parser ()
lineComment = do
  _ <- string "--"
  _ <- manyTill anyChar (Control.Monad.void newline <|> eof)
  return ()

blockComment :: Parser ()
blockComment = do
  _ <- string "{-"
  _ <- manyTill anyChar (try $ string "-}")
  return ()

comment :: Parser ()
comment = try lineComment <|> blockComment

skipCommentsAndWhitespace :: Parser ()
skipCommentsAndWhitespace = skipMany (Control.Monad.void (string " ") <|> comment <|> Control.Monad.void newline)

pProg :: Parser RProg
pProg =
  skipCommentsAndWhitespace
    >> RProg <$> many pTLStmt

pTLStmt :: Parser RTLStmt
pTLStmt =
  choice
    [ try pTLExp,
      try pTLFunc,
      pTLEnum
    ]
    <?> "top level statement"

pTLExp :: Parser RTLStmt
pTLExp = do
  _ <- pReserved resDef
  typedName <- try pTypedName <|> pUntypedName
  _ <- pReserved resAssign
  body <- pExpr
  return $ RTLExp typedName body

pTLFunc :: Parser RTLStmt
pTLFunc = do
  _ <- pReserved resDef
  name <- pIdentifier
  typeArgs <- many (pAngles pIdentifier)
  args <- many1 (pParens pTypedName)
  t <-
    do
      _ <- pReserved resTyping
      ty <- pType
      return (Just ty)
      <|> return Nothing
  _ <- pReserved resAssign
  body <- pExpr
  return $ RTLFunc name typeArgs args body t

pTLEnum :: Parser RTLStmt 
pTLEnum = do 
  _ <- pReserved resEnum 
  enumName <- pIdentifier 
  variants <- pBraces (sepBy pVariant (pReserved resComma))
  return $ RTLEnum enumName variants 

pVariant :: Parser (String, [RType])
pVariant = do 
  variantName <- pIdentifier 
  fields <- pBrackets (sepBy pType (pReserved resComma))
  return (variantName, fields)

pExpr :: Parser RExpr
pExpr =
  buildExpressionParser opTable pExpr' <|> pExpr'
    <?> "expression"

pExpr' = try pApp <|> pExpr'' <?> "expression"

pExpr'' = pParens pExpr <|> pAtom <?> "expression"

pApp :: Parser RExpr
pApp = do
  e <- pExpr''
  es <- many1 pExpr''
  return $ foldl (REBin " ") e es

pAtom =
  choice
    [ try pLet,
      try pLetrec,
      try pIf,
      try pLam,
      try pMatch, 
      try pInt,
      try pTypeApp,
      pString,
      pId
    ]
    <?> "atom"

pTypeApp = do 
  _ <- pReserved resAt
  ty <- pType
  return $ RETypeApp ty

pInt = natural lexer >>= \i -> return $ REInt (fromIntegral i)

pField = do
  fieldName <- pIdentifier
  _ <- pReserved resAssign
  fieldExpr <- pExpr
  return (fieldName, fieldExpr)

pString = REString <$> Text.Parsec.Token.stringLiteral lexer

pId = REId <$> pIdentifier

pType :: Parser RType
pType =
  try (buildExpressionParser [[Infix pArrowType AssocRight]] pType')
    <?> "type"

pType' :: Parser RType
pType' = pParens pType <|> pTypeAtom <?> "type"

pTypeAtom = pTypeAll <|> pTypeId <?> "type atom"

pTypeAll = do
  name <- pAngles pIdentifier
  ty <- pType
  return $ RTAll name ty

pTypeId = do
  id' <- pIdentifier
  return $ RTId id'

pArrowType = do
  _ <- pReserved resArrow
  return RTFunc

pUntypedName :: Parser TypedName
pUntypedName = do
  id' <- pIdentifier
  return (TypedName id' Nothing)

pTypedName :: Parser TypedName
pTypedName = do
  id' <- pIdentifier
  _ <- pReserved resTyping
  ty <- pType
  return (TypedName id' (Just ty))

pLet = do
  _ <- pReserved resLet
  letClauses <- pLetClause `sepBy1` pReserved resComma
  _ <- pReserved resIn
  e2 <- pExpr
  return $ RELet letClauses e2

pLetClause :: Parser (TypedName, RExpr)
pLetClause = do
  id' <- try pTypedName <|> pUntypedName
  _ <- pReserved resAssign
  e1 <- pExpr
  return (id', e1)

pLetrec = do
  _ <- pReserved resLetrec
  letrecClauses <- pLetrecClause `sepBy1` pReserved resComma
  _ <- pReserved resIn
  e2 <- pExpr
  return $ RELetrec letrecClauses e2

pLetrecClause :: Parser (TypedName, RExpr)
pLetrecClause = do
  id' <- try pTypedName <|> pUntypedName
  _ <- pReserved resAssign
  e1 <- pExpr
  return (id', e1)

pLam = do
  _ <- pReserved resLam
  typeArgs <- many (pAngles pIdentifier)
  arg <- many1 (pParens pTypedName <|> pUntypedName)
  t <-
    do
      _ <- pReserved resTyping
      ty <- pType
      return (Just ty)
      <|> return Nothing
  _ <- pReserved resFat
  body <- pExpr
  return $ RELam typeArgs arg body t

pIf = do
  _ <- pReserved resIf
  cond <- pExpr
  _ <- pReserved resThen
  e1 <- pExpr
  _ <- pReserved resElse
  e2 <- pExpr
  return $ REIf cond e1 e2

pMatch = do 
  _ <- pReserved resMatch 
  e0 <- pExpr 
  branches <- pBraces (sepBy pBranch (pReserved resComma))
  return $ REMatch e0 branches

pBranch = do 
  cons <- pIdentifier 
  args <- many pIdentifier
  _ <- pReserved resFat 
  body <- pExpr 
  return $ RBranch cons args body
    
laneReservedNames =
  [ resLet,
    resLetrec,
    resIn,
    resIf,
    resThen,
    resElse,
    resLam,
    resDef,
    resEnum,
    resMatch
  ]

laneReservedOps =
  [ resTyping,
    resFat,
    resSemi,
    resComma,
    resDot,
    resArrow,
    resEq,
    resNeq,
    resLt,
    resGt,
    resLeq,
    resGeq,
    resMul,
    resDiv,
    resAdd,
    resSub,
    resAssign,
    resAt
  ]

resArrow = "->"

resEq = "=="

resNeq = "!="

resLt = "<"

resGt = ">"

resLeq = "<="

resGeq = ">="

resMul = "*"

resDiv = "/"

resAdd = "+"

resSub = "-"

resAssign = "="

resLet = "let"

resLetrec = "letrec"

resIn = "in"

resIf = "if"

resThen = "then"

resElse = "else"

resLam = "fn"

resDef = "def"

resSpace = " "

resTyping = ":"

resFat = "=>"

resSemi = ";"

resComma = ","

resDot = "."

resEnum = "enum"

resMatch = "match"

resAt = "@"

opTable =
  [ infixLeftOps [resEq, resNeq, resLt, resGt, resLeq, resGeq],
    infixLeftOps [resMul, resDiv],
    infixLeftOps [resAdd, resSub]
  ]

infixLeftOps :: [String] -> [Operator String () Identity RExpr]
infixLeftOps = map (\op -> Infix (pOp op) AssocLeft)

pOp name = do
  _ <- pReservedOp name
  return $ REBin name

examples =
  [ ( "def main = x + y / z",
      [ RTLExp
          (TypedName "main" Nothing)
          ( REBin
              resAdd
              (REId "x")
              (REBin resDiv (REId "y") (REId "z"))
          )
      ]
    )
  ]

checkExamples = map (\(s, r) -> (s, parseLaneProg s == Right (RProg r))) examples

showExamples = mapM_ pretty checkExamples
  where
    pretty (s, r) = putStrLn (if r then "pass" else "fail: " ++ s)