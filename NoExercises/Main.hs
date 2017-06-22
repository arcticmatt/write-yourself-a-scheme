{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import System.Environment
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

main :: IO ()
main = getArgs >>= print . eval . readExpr . head


-- *** Data Structures & Types ***
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String  -- Constructors and types have different namespaces!
             | Bool Bool
             deriving (Eq)

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name)       = name
  show (Number contents) = show contents
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (List contents)   = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t)  = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
   [("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("-", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
   ]


-- *** Lexer Code ***
-- Space consumer
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = L.skipLineComment ";"
        blockCmnt = L.skipBlockComment "#|" "|#"


-- *** Character Parsing ***
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

openP :: Parser Char
openP = char '('

closeP :: Parser Char
closeP = char ')'

-- *** LispVal Parsing ***
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escaped <|> noneOf "\\\""
  char '"'
  return $ String x

escaped :: Parser Char
escaped = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '\"' -> x
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    _    -> error "unsupported escape"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> symbol
  rest  <- many (letterChar <|> digitChar <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

-- TODO: Floats, #x/o/d, etc.
parseNumber :: Parser LispVal
parseNumber = Number . read <$> some digitChar

parseEitherList :: Parser LispVal
parseEitherList = do
  openP
  l <- try parseList <|> parseDottedList
  closeP
  return l

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr sc

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr sc
  t <- char '.' >> sc >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- *** Expression Parsing ***
parseExpr :: Parser LispVal
parseExpr =
      parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parseEitherList

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "No match: " ++ show err
  Right val -> val



-- *** Evaluation ***
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- Takes a primitive Haskell function (often an operator section) and wraps it
-- with code to unpack an argument list, apply the function to it, and
-- wrap the result up in our Number constructor.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- Weak typing. If we can interpret strings or lists as a single number, do it.
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
  if null parsed
    then 0 -- TODO: throw error?
    else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
