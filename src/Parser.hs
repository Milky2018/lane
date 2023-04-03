{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Parser (
  parseLaneProg
) where

import Text.ParserCombinators.Parsec
import Numeric
import Control.Applicative (empty)
import Text.Parsec.Expr
import Raw (RProg (..), RExpr (..), RType (..), TypedName (..), RTLStmt (..))

identifier :: Parser String
identifier = do
  first <- letter <|> char '_'
  rest <- many (letter <|> digit <|> char '_')
  let name = first:rest
  if name `elem` laneReserved
    then unexpected ("reserved word " ++ show name)
    else return name

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  spaces
  res <- p
  spaces
  _ <- char ')'
  return res

parseLaneProg :: String -> Either ParseError RProg
parseLaneProg = parse laneParser "(unknown)"

laneParser :: Parser RProg
laneParser = spaces >> pProg

pProg :: Parser RProg
pProg = RProg <$> (sepBy pTLStmt (spaces *> many newline <* spaces) <* eof)

pTLStmt :: Parser RTLStmt
pTLStmt = choice [
    try pTLExp
  , try pTLFunc
  ] <?> "top level statement"

pTLExp :: Parser RTLStmt
pTLExp = do
  _ <- string resDef
  spaces
  typedName <- pTypedName
  spaces
  _ <- string resAssign
  spaces
  body <- pExpr
  return $ RTLExp typedName body

pTLFunc :: Parser RTLStmt
pTLFunc = do
  _ <- string resDef
  spaces
  name <- identifier
  spaces
  args <- many1 (Parser.parens pTypedName <* spaces)
  t <-
    do
    _ <- string resTyping
    spaces
    ty <- pType
    return (Just ty)
    <|>
    return Nothing
  spaces
  _ <- string resAssign
  spaces
  body <- pExpr
  return $ RTLFunc name args body t

pExpr :: Parser RExpr
pExpr = expr <* spaces where
  expr = buildExpressionParser opTable pExpr' <|> atom <?> "expression"

pExpr' :: Parser RExpr
pExpr' = expr <* spaces where
  expr = try pApp <|> pExpr'' <|> atom <?> "expression"

pExpr'' :: Parser RExpr
pExpr'' = expr <* spaces where
  expr = Parser.parens pExpr <|> atom <?> "expression"

atom = choice [
      try pLet
    , try pIf
    , try pLam
    , try pUnit
    , try pBool
    , pInt
    , pString
    , pId
    ] <?> "atom"

eol :: Parser String
eol =
      try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

spacef = spaces
         *> notFollowedBy (choice $ map (try . string) laneReserved)
         >> return (REBin resSpace)

pApp = buildExpressionParser [[Infix spacef AssocLeft]] pExpr''

pUnit = string resUnit >> return REUnit

pBool = (string resTrue >> return (REBool True))
    <|> (string resFalse >> return (REBool False))

pInt = do s <- getInput
          case {-# readSigned #-} readDec s of
            [(n, s')] -> REInt n <$ setInput s'
            _ -> empty

pString = between (char '\"') (char '\"') (many pStringChar) >>= \s -> return (REString s)

pStringChar :: Parser Char
pStringChar = char '\\' *> (escape <|> unicode)
              <|> satisfy (`notElem` "\"\\")

escape :: Parser Char
escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where decode :: Char -> Char -> Parser Char
        decode c r = r <$ char c

unicode :: Parser Char
unicode = char 'u' *> (decode <$> count 4 hexDigit)
  where decode x = toEnum code
          where code = case readHex x of
                  ((c,_):_) -> c
                  _ -> error "unicode decode error"


pId :: Parser RExpr
pId = REId <$> Parser.identifier

pType :: Parser RType
pType = ty <* spaces where
  ty = try (buildExpressionParser [[Infix pArrowType AssocRight]] pType')
    <|> pTypeAtom
    <?> "type"

pType' :: Parser RType
pType' = ty <* spaces where
  ty = Parser.parens pType <|> pTypeAtom <?> "type"

pTypeAtom = choice
  [ try $ string resTBool >> return RTBool
  , try $ string resTInt >> return RTInt
  , try $ string resTString >> return RTString
  , try $ string resTUnit >> return RTUnit
  ]

pArrowType = do
  spaces
  _ <- string resArrow
  spaces
  return RTFunc

pTypedName :: Parser TypedName
pTypedName = do
  id' <- Parser.identifier
  spaces
  (
    do
    _ <- string resTyping
    spaces
    ty <- pType
    return (TypedName id' (Just ty))) <|>
    return (TypedName id' Nothing)

pLet = do
  _ <- string resLet
  spaces
  letClauses <- pLetClause `sepBy1` (string resComma <* spaces)
  spaces
  _ <- string resIn
  spaces
  e2 <- pExpr
  return $ RELet letClauses e2

pLetClause :: Parser (TypedName, RExpr)
pLetClause = do
  id' <- pTypedName
  spaces
  _ <- string resAssign
  spaces
  e1 <- pExpr
  return (id', e1)

pLam = do
  _ <- string resLam
  spaces
  arg <- many1 (Parser.parens pTypedName <* spaces)
  t <-
    do
    _ <- string resTyping
    spaces
    ty <- pType
    return (Just ty)
    <|>
    return Nothing
  spaces
  _ <- string resAssign
  spaces
  body <- pExpr
  return $ RELam arg body t

pIf = do
  _ <- string resIf
  spaces
  cond <- pExpr
  spaces
  _ <- string resThen
  spaces
  e1 <- pExpr
  spaces
  _ <- string resElse
  spaces
  e2 <- pExpr
  return $ REIf cond e1 e2

pOp ops = do
  spaces
  op <- choice (map (try . string) ops)
  spaces
  return $ REBin op

laneReserved =
  [ resArrow
  , resEq
  , resNeq
  , resLt
  , resGt
  , resLeq
  , resGeq
  , resMul
  , resDiv
  , resAdd
  , resSub
  , resAssign
  , resLet
  , resIn
  , resIf
  , resThen
  , resElse
  , resLam
  , resDef
  , resTrue
  , resFalse
  , resUnit
  , resSpace
  , resTBool
  , resTUnit
  , resTInt
  , resTString
  , resTyping
  , resFat
  , resSemi
  , resComma
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
resIn = "in"
resIf = "if"
resThen = "then"
resElse = "else"
resLam = "fun"
resDef = "def"
resTrue = "true"
resFalse = "false"
resUnit = "unit"
resSpace = " "
resTBool = "Bool"
resTUnit = "Unit"
resTInt = "Int"
resTString = "String"
resTyping = ":"
resFat = "=>"
resSemi = ";"
resComma = ","

opTable =
  [ [Infix (pOp [resEq, resNeq, resLt, resGt, resLeq, resGeq]) AssocLeft]
  , [Infix (pOp [resMul, resDiv]) AssocLeft]
  , [Infix (pOp [resAdd, resSub]) AssocLeft]
  ]

examples =
  [ ("def main => x + y / z"
  , [RTLExp
      (TypedName "main" Nothing)
      (REBin
        resAdd
        (REId "x")
        (REBin resDiv (REId "y") (REId "z")))])
  ]

checkExamples = map (\(s, r) -> (s, parseLaneProg s == Right (RProg r))) examples

showExamples = mapM_ pretty checkExamples where
  pretty (s, r) = putStrLn (if r then "pass" else "fail: " ++ s)


