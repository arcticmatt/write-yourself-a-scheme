{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where
import System.Environment
import Control.Applicative (liftA2)
import Numeric
import Data.Char (digitToInt, toLower)
import qualified Data.Map as M
import Data.Ratio
import Data.Complex
import Control.Monad (void)
import qualified Data.Vector as V

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | String String
             | Bool Bool
             | C Character
             | Complex (Complex Double)
             | Float Float
             | Ratio Rational
             | Integer Integer
             | Vec (V.Vector LispVal) -- immutable so not true constant access
             deriving (Eq)

instance Show LispVal where
  show (String contents)  = "\"" ++ contents ++ "\""
  show (Atom name)        = name
  show (Integer contents) = show contents
  show (Float contents)   = show contents
  show (Ratio contents)   = show contents
  show (Complex contents) = show contents
  show (C c)              = show c
  show (Bool True)        = "#t"
  show (Bool False)       = "#f"
  show (List contents)    = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t)   = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
  show (Vec contents)     = "#(" ++ unwordsList (V.toList contents) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

data Character =
    Single Char
  | ESC
  | US
  | BS
  | SUB
  | LF
  | FF
  | CR
  | DEL
  | SPACE
  | HT deriving (Eq, Show)

type CharacterName = String

characterMap :: M.Map CharacterName Character
characterMap = M.fromList
   [("altmode", ESC)
  , ("backnext", US)
  , ("backspace", BS)
  , ("call", SUB)
  , ("linefeed", LF)
  , ("newline", LF)
  , ("page", FF)
  , ("return", CR)
  , ("rubout", DEL)
  , ("space", SPACE)
  , ("tab", HT)
   ]

-- *** Lexer Code ***
-- Space consumer
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = L.skipLineComment ";"
        blockCmnt = L.skipBlockComment "#|" "|#"

-- We'll follow the strategy where whitespace will be consumed *after* every lexeme
-- automatically, but not before it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- *** String Parsing ***
-- "   \"  yo, we have this part \\\"nested\\\\"  \"    "
-- "\"yo, we have this part \\\"nested\\\\"\""
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\\\"")
  char '"'
  return $ String x

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '\"' -> x
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    _    -> error "unsupported escape"

-- *** Atom Parsing ***
parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> symbol
  rest  <- many (letterChar <|> digitChar <|> symbol)
  let atom = first:rest
  return $ Atom atom

-- *** Boolean Parsing ***
parseBool :: Parser LispVal
parseBool = do
  char '#'
  x <- char 't' <|> char 'f'
  if x == 't' then return (Bool True) else return (Bool False)

-- *** Character Parsing ***
parseCharacter :: Parser LispVal
parseCharacter = do
  char '#'
  char '\\'
  try parseCharacter' <|> parseCharacterName

-- Parses <character> patterns
parseCharacter' :: Parser LispVal
parseCharacter' = do
  x <- anyChar
  notFollowedBy alphaNumChar
  return $ C (Single x)

-- Parses <character name> patterns
parseCharacterName :: Parser LispVal
parseCharacterName = do
  x <- some letterChar
  let name = M.lookup (toLower <$> x) characterMap -- ignore case
  case name of
    Just name' -> return $ C name'
    _          -> error "invalid character name"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

openP :: Parser Char
openP = char '('

closeP :: Parser Char
closeP = char ')'

-- ***** Number Parsing *****
-- TODO: precision, signs
-- *** Integer Parsing ***
parseInteger :: Parser LispVal
parseInteger = parseRegInteger
  <|> parseBaseInteger

parseRegInteger :: Parser LispVal
parseRegInteger = Integer . read <$> some digitChar

parseBaseInteger :: Parser LispVal
-- parseNumber = fmap (Number . read) $ some digitChar
-- parseNumber = some digitChar >>= return . Number . read
parseBaseInteger = do
  char '#'
  base <- oneOf "bodx"
  case base of
    'b' -> Integer . fst . head . readBin <$> some digitChar
    'o' -> Integer . fst . head . readOct <$> some digitChar
    'x' -> Integer . fst . head . readHex <$> some digitChar
    'd' -> Integer . read <$> some digitChar
    _   -> error "shouldn't reach this"

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (liftA2 (||) (=='1') (=='0')) digitToInt

-- *** Float/Real Parsing ***
parseFloat :: Parser LispVal
parseFloat = do
  before <- some digitChar -- disallow .45
  char '.'
  after <- some digitChar -- disallow 45.
  return . Float . fst . head . readFloat $ (before ++ "." ++ after)

-- *** Rational Parsing ***
parseRational :: Parser LispVal
parseRational = do
  num <- lexeme parseInteger
  lexeme $ char '/' -- TODO: whitespace
  denom <- lexeme parseInteger
  return $ Ratio (lispToInteger num % lispToInteger denom)

lispToInteger :: LispVal -> Integer
lispToInteger (Integer x) = x
lispToInteger _           = error "lispToInteger error"

-- *** Complex Parsing ***
parseComplex :: Parser LispVal
parseComplex = lexeme $ do
  real <- lexeme $ try parseFloat <|> parseInteger
  lexeme $ char '+' -- TODO: whitespace
  imag <- try parseFloat <|> parseInteger
  char 'i'
  return $ Complex (lispToDouble real :+ lispToDouble imag)

lispToDouble :: LispVal -> Double
lispToDouble (Float f)   = realToFrac f
lispToDouble (Integer n) = fromIntegral n
lispToDouble _           = error "lispToDouble error"

-- *** List Parsing ***
-- Regular list
parseList :: Parser LispVal
parseList = between openP closeP (try parseList' <|> parseDottedList)

parseList' :: Parser LispVal
parseList' = List <$> sepBy parseExpr sc

-- Dotted list
parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr sc
  t <- char '.' >> sc >> parseExpr
  return $ DottedList h t

-- *** Quotes ***
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  char '`' -- note: must escape this https://unix.stackexchange.com/questions/48392/understanding-backtick
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- Vectors...
parseVector :: Parser LispVal
parseVector = do
  char '#'
  items <- between openP closeP (sepBy parseExpr sc)
  return $ Vec $ V.fromList items

-- *** Run Functions ***
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> try parseBool    -- uses '#'
  <|> try parseCharacter
  <|> try parseComplex
  <|> try parseFloat
  <|> try parseRational
  <|> try parseInteger -- uses '#'
  <|> parseVector      -- uses '#'
  <|> parseQuoted
  <|> parseQuasiquoted
  <|> parseUnquote
  <|> parseList

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn expr
  putStrLn (readExpr expr)
