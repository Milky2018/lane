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

braces :: Parser a -> Parser a
braces p = do
  _ <- char '{'
  spaces
  res <- p
  spaces
  _ <- char '}'
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
  , try pTLStruct
  ] <?> "top level statement"

pTLExp :: Parser RTLStmt
pTLExp = do
  _ <- string resDef
  spaces
  typedName <- try pTypedName <|> pUntypedName
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

-- struct S { field1 : t1, field2 : t2, ... }
pTLStruct :: Parser RTLStmt 
pTLStruct = do 
  _ <- string resStruct 
  spaces 
  structName <- identifier
  spaces
  fields <- Parser.braces (sepBy pTypedName (spaces *> string resComma *> spaces))
  return $ RTLStruct structName fields

pExpr :: Parser RExpr
pExpr = expr <* spaces where
  expr = buildExpressionParser opTable pExpr' <?> "expression"

pExpr' :: Parser RExpr
pExpr' = expr <* spaces where
  expr = try pApp <?> "expression"

pExpr'' :: Parser RExpr 
pExpr'' = expr <* spaces where 
  expr = try pAccess <|> pExpr''' <?> "expression"

pExpr''' :: Parser RExpr
pExpr''' = expr <* spaces where
  expr = Parser.parens pExpr <|> atom <?> "expression"

atom = choice [
      try pLet
    , try pIf
    , try pLam
    , try pStructCons 
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

pAccess = do
  e <- pExpr'''
  accessors <- many1 (string resDot >> identifier)
  return $ foldl REAccess e accessors

pInt = do s <- getInput
          case {-# readSigned #-} readDec s of
            [(n, s')] -> REInt n <$ setInput s'
            _ -> empty

-- S { field1 = e1, field2 = e2, ... }
pStructCons = do 
  structName <- identifier
  spaces
  fields <- Parser.braces (sepBy pField (spaces *> string resComma *> spaces))
  return $ REStructCons structName fields

pField = do 
  fieldName <- identifier
  spaces
  _ <- string resAssign
  spaces
  fieldExpr <- pExpr
  return (fieldName, fieldExpr)

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
    <?> "type"

pType' :: Parser RType
pType' = ty <* spaces where
  ty = Parser.parens pType <|> pTypeAtom <?> "type"

pTypeAtom = choice
  [ try $ string resTBool >> return RTBool
  , try $ string resTInt >> return RTInt
  , try $ string resTString >> return RTString
  , try $ string resTUnit >> return RTUnit
  , identifier >>= \name -> return $ RTId name 
  ] <?> "type atom"

pArrowType = do
  spaces
  _ <- string resArrow
  spaces
  return RTFunc

pUntypedName :: Parser TypedName 
pUntypedName = do 
  id' <- Parser.identifier
  return (TypedName id' Nothing)

pTypedName :: Parser TypedName
pTypedName = do
  id' <- Parser.identifier
  spaces 
  _ <- string resTyping
  spaces
  ty <- pType
  return (TypedName id' (Just ty))

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
  id' <- try pTypedName <|> pUntypedName
  spaces
  _ <- string resAssign
  spaces
  e1 <- pExpr
  return (id', e1)

pLam = do
  _ <- string resLam
  spaces
  arg <- many1 (Parser.parens pTypedName <|> pUntypedName <* spaces)
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
  , resSpace
  , resTBool
  , resTUnit
  , resTInt
  , resTString
  , resTyping
  , resFat
  , resSemi
  , resComma
  , resDot 
  , resStruct
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
resSpace = " "
resTBool = "Bool"
resTUnit = "Unit"
resTInt = "Int"
resTString = "String"
resTyping = ":"
resFat = "=>"
resSemi = ";"
resComma = ","
resDot = "."
resStruct = "struct"

opTable =
  [ [Infix (pOp [resEq, resNeq, resLt, resGt, resLeq, resGeq]) AssocLeft]
  , [Infix (pOp [resMul, resDiv]) AssocLeft]
  , [Infix (pOp [resAdd, resSub]) AssocLeft]
  ]

examples =
  [ ("def main = x + y / z"
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