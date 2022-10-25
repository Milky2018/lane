{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser (
  parseLaneProg
) where

import Raw

import Text.ParserCombinators.Parsec
import Numeric
import Control.Applicative (empty)
import Text.Parsec.Language
import Text.Parsec.Expr
import Data.Char (isDigit)
import Text.ParserCombinators.Parsec.Token

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
  char '('
  spaces
  res <- p
  spaces
  char ')'
  return res

parseLaneProg :: String -> Either ParseError RProg
parseLaneProg = parse laneParser "(unknown)"

laneParser :: Parser RProg
laneParser = spaces >> pProg

pProg :: Parser RProg
pProg = RProg <$> (pExpr <* eof)

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
         >> return (RExprBin resSpace)

pApp = buildExpressionParser [[Infix spacef AssocLeft]] pExpr''

pUnit = string resUnit >> return RExprUnit

pBool = (string resTrue >> return (RExprBool True))
    <|> (string resFalse >> return (RExprBool False))

pInt = do s <- getInput
          case {-# readSigned #-} readDec s of
            [(n, s')] -> RExprInt n <$ setInput s'
            _ -> empty

pString = between (char '\"') (char '\"') (many pStringChar) >>= \s -> return (RExprString s)

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
          where ((code,_):_) = readHex x

pId :: Parser RExpr
pId = RExprId <$> Parser.identifier

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
  string resImpl
  spaces
  return RTFunc

pTypedName :: Parser TypedName
pTypedName = do
  id <- Parser.identifier
  spaces
  (
    do
    string resTyping
    spaces
    ty <- pType
    return (TypedName id (Just ty))) <|>
    return (TypedName id Nothing)

pLet = do
  string resLet
  spaces
  letClauses <- pLetClause `sepBy1` (char ';' <* spaces)
  spaces
  string resIn
  spaces
  e2 <- pExpr
  return $ RExprLet letClauses e2

pLetClause :: Parser (TypedName, RExpr)
pLetClause = do
  id <- pTypedName
  spaces
  string resAssign
  spaces
  e1 <- pExpr
  return (id, e1)

pLam = do
  string resLam
  spaces
  arg <- many1 (Parser.parens pTypedName <* spaces)
  t <- 
    do
    string resTyping
    spaces
    ty <- pType
    return (Just ty) 
    <|>
    return Nothing
  spaces 
  string resFat
  spaces
  body <- pExpr
  return $ RExprLam arg body t

pIf = do
  string resIf
  spaces
  cond <- pExpr
  spaces
  string resThen
  spaces
  e1 <- pExpr
  spaces
  string resElse
  spaces
  e2 <- pExpr
  return $ RExprIf cond e1 e2

pOp ops = do
  spaces
  op <- choice (map (try . string) ops)
  spaces
  return $ RExprBin op

laneReserved =
  [ resImpl
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
  ]

resImpl = "->"
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

opTable =
  [ [Infix (pOp [resEq, resNeq, resLt, resGt, resLeq, resGeq]) AssocLeft]
  , [Infix (pOp [resMul, resDiv]) AssocLeft]
  , [Infix (pOp [resAdd, resSub]) AssocLeft]
  ]

examples =
  [ ("x + y / z", RExprBin resAdd (RExprId "x") (RExprBin resDiv (RExprId "y") (RExprId "z")))
  , ("let x = 1 in let y = 2 in x + y", RExprLet [(TypedName "x" Nothing,RExprInt 1)] (RExprLet [(TypedName "y" Nothing,RExprInt 2)] (RExprBin "+" (RExprId "x") (RExprId "y"))))
  , ("x + y  * z w", RExprBin resAdd (RExprId "x") (RExprBin resMul (RExprId "y") (RExprBin resSpace (RExprId "z") (RExprId "w"))))
  , ("(x+y) * z w", RExprBin resMul (RExprBin resAdd (RExprId "x") (RExprId "y")) (RExprBin resSpace (RExprId "z") (RExprId "w")))
  , ("operator operand", RExprBin resSpace (RExprId "operator") (RExprId "operand"))
  , ("if true then 1 else 2", RExprIf (RExprBool True) (RExprInt 1) (RExprInt 2))
  , ("let x = 1; y = 2; z = 3 in x + y + z", RExprLet [(TypedName "x" Nothing,RExprInt 1),(TypedName "y" Nothing,RExprInt 2),(TypedName "z" Nothing,RExprInt 3)] (RExprBin "+" (RExprBin "+" (RExprId "x") (RExprId "y")) (RExprId "z")))
  , ("(fun (x : Int) => x + y) 10", RExprBin " " (RExprLam [TypedName "x" (Just RTInt)] (RExprBin "+" (RExprId "x") (RExprId "y")) Nothing) (RExprInt 10))
  , ("(fun (x : Int) : Int => x + y) 10", RExprBin " " (RExprLam [TypedName "x" (Just RTInt)] (RExprBin "+" (RExprId "x") (RExprId "y")) (Just RTInt)) (RExprInt 10))
  ]

checkExamples = map (\(s, r) -> (s, parseLaneProg s == Right (RProg r))) examples

showExamples = mapM_ pretty checkExamples where
  pretty (s, r) = putStrLn (if r then "pass" else "fail: " ++ s ++ "")