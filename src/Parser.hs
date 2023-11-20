{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser
  ( parseLaneProg,
  )
where

import Control.Monad (void)
import Raw (RExpr (..), RProg (..), RTLStmt (..), RType (..), TypedName (..))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (LanguageDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser (..), TokenParser, makeTokenParser)

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
      try pTLStruct
    ]
    <?> "top level statement"

pTLExp :: Parser RTLStmt
pTLExp = do
  _ <- Parser.pReserved resDef
  typedName <- try pTypedName <|> pUntypedName
  _ <- Parser.pReserved resAssign
  body <- pExpr
  return $ RTLExp typedName body

pTLFunc :: Parser RTLStmt
pTLFunc = do
  _ <- Parser.pReserved resDef
  name <- Parser.pIdentifier
  args <- many1 (Parser.pParens pTypedName)
  t <-
    do
      _ <- Parser.pReserved resTyping
      ty <- pType
      return (Just ty)
      <|> return Nothing
  _ <- Parser.pReserved resAssign
  body <- pExpr
  return $ RTLFunc name args body t

pTLStruct :: Parser RTLStmt
pTLStruct = do
  _ <- Parser.pReserved resStruct
  structName <- Parser.pIdentifier
  fields <- Parser.pBraces (sepBy pTypedName (Parser.pReserved resComma))
  return $ RTLStruct structName fields

pExpr :: Parser RExpr
pExpr =
  buildExpressionParser opTable pExpr' <|> pExpr'
    <?> "expression"

pExpr' = try pApp <|> pExpr'' <?> "expression"

pExpr'' = try pAccess <|> pExpr''' <?> "expression"

pExpr''' = Parser.pParens pExpr <|> pAtom <?> "expression"

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
      try pStructCons,
      pInt,
      pString,
      pId
    ]
    <?> "atom"

pAccess = do
  e <- pExpr'''
  accessors <- many1 (Text.Parsec.string resDot >> Parser.pIdentifier)
  return $ foldl REAccess e accessors

pInt = natural lexer >>= \i -> return $ REInt (fromIntegral i)

pStructCons = do
  structName <- Parser.pIdentifier
  fields <- Parser.pBraces (sepBy pField (Parser.pReserved resComma))
  return $ REStructCons structName fields

pField = do
  fieldName <- Parser.pIdentifier
  _ <- Parser.pReserved resAssign
  fieldExpr <- pExpr
  return (fieldName, fieldExpr)

pString = REString <$> Text.Parsec.Token.stringLiteral lexer

pId = REId <$> Parser.pIdentifier

pType :: Parser RType
pType =
  try (buildExpressionParser [[Infix pArrowType AssocRight]] pType')
    <?> "type"

pType' :: Parser RType
pType' = Parser.pParens pType <|> pTypeAtom <?> "type"

pTypeAtom = (Parser.pIdentifier >>= \name -> return $ RTId name) <?> "type atom"

pArrowType = do
  _ <- Parser.pReserved resArrow
  return RTFunc

pUntypedName :: Parser TypedName
pUntypedName = do
  id' <- Parser.pIdentifier
  return (TypedName id' Nothing)

pTypedName :: Parser TypedName
pTypedName = do
  id' <- Parser.pIdentifier
  _ <- Parser.pReserved resTyping
  ty <- pType
  return (TypedName id' (Just ty))

pLet = do
  _ <- Parser.pReserved resLet
  letClauses <- pLetClause `sepBy1` Parser.pReserved resComma
  _ <- Parser.pReserved resIn
  e2 <- pExpr
  return $ RELet letClauses e2

pLetClause :: Parser (TypedName, RExpr)
pLetClause = do
  id' <- try pTypedName <|> pUntypedName
  _ <- Parser.pReserved resAssign
  e1 <- pExpr
  return (id', e1)

pLetrec = do
  _ <- Parser.pReserved resLetrec
  letrecClauses <- pLetrecClause `sepBy1` Parser.pReserved resComma
  _ <- Parser.pReserved resIn
  e2 <- pExpr
  return $ RELetrec letrecClauses e2

pLetrecClause :: Parser (TypedName, RExpr)
pLetrecClause = do
  id' <- try pTypedName <|> pUntypedName
  _ <- Parser.pReserved resAssign
  e1 <- pExpr
  return (id', e1)

pLam = do
  _ <- Parser.pReserved resLam
  arg <- many1 (Parser.pParens pTypedName <|> pUntypedName)
  t <-
    do
      _ <- Parser.pReserved resTyping
      ty <- pType
      return (Just ty)
      <|> return Nothing
  _ <- Parser.pReserved resFat
  body <- pExpr
  return $ RELam arg body t

pIf = do
  _ <- Parser.pReserved resIf
  cond <- pExpr
  _ <- Parser.pReserved resThen
  e1 <- pExpr
  _ <- Parser.pReserved resElse
  e2 <- pExpr
  return $ REIf cond e1 e2

laneReservedNames =
  [ resLet,
    resLetrec,
    resIn,
    resIf,
    resThen,
    resElse,
    resLam,
    resDef,
    resStruct
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
    resAssign
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

resStruct = "struct"

opTable =
  [ infixLeftOps [resEq, resNeq, resLt, resGt, resLeq, resGeq],
    infixLeftOps [resMul, resDiv],
    infixLeftOps [resAdd, resSub]
  ]

infixLeftOps = map (\op -> Infix (pOp op) AssocLeft)

pOp name = do
  _ <- Parser.pReservedOp name
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