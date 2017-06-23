{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import System.Environment
import Control.Monad (void)
import Control.Exception

import Text.Megaparsec as TM
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  print evaled


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

data LispException =
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser (ParseError (Token String) Dec)
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Exception LispException

instance Show LispException where
  show (UnboundVar message varname)    = message ++ ": " ++ varname
  show (BadSpecialForm message form)   = message ++ ": " ++ show form
  show (NotFunction message func)      = message ++ ": " ++ show func
  show (NumArgs expected found)        = "Expected " ++ show expected
                                         ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found)   = "Invalid type: expected " ++ expected
                                         ++ ", found " ++ show found
  show (Parser parseErr)               = "Parse error at " ++ show parseErr
  show (Default s)                     = "Default: " ++ s

type ThrowsException = Either LispException

-- Uses existential quantification
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsException a)

primitives :: [(String, [LispVal] -> ThrowsException LispVal)]
primitives =
   [("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("-", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , ("<=", numBoolBinop (<=))
  , (">=", numBoolBinop (>=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
   ]


-- *** Exception Code ***
-- trapException action = catch action (return . show)

-- extractValue :: ThrowsException a -> a
-- extractValue (Right r) = r
-- extractValue (Left l)  = l

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
  l <- TM.try parseList <|> parseDottedList
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

readExpr :: String -> ThrowsException LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> Left $ Parser err
  Right val -> return val


-- *** Evaluation ***
eval :: LispVal -> ThrowsException LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, conseq, alt]) =
  do result <- eval predicate
     case result of
       Bool False -> eval alt
       Bool True  -> eval conseq
       x          -> Left $ TypeMismatch "bool" x
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = Left $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsException LispVal
apply func args = maybe (Left $ NotFunction "Unrecognized primitive function args" func)
 ($ args) (lookup func primitives)

-- Takes a primitive Haskell function (often an operator section) and wraps it
-- with code to unpack an argument list, apply the function to it, and
-- wrap the result up in our Number constructor.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsException LispVal
numericBinop _ [] = Left $ NumArgs 2 []
numericBinop _ [singleVal] = Left $ NumArgs 2 [singleVal]
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> ThrowsException a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsException LispVal
boolBinop _ _ args | length args /= 2 = Left $ NumArgs 2 args
boolBinop unpacker op [l, r] = do left <- unpacker l
                                  right <- unpacker r
                                  return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsException LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsException LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsException LispVal
boolBoolBinop = boolBinop unpackBool

-- Weak typing
unpackNum :: LispVal -> ThrowsException Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then Left $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = Left $ TypeMismatch "number" notNum

-- Weak typing
unpackStr :: LispVal -> ThrowsException String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = Left $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsException Bool
unpackBool (Bool b) = return b
unpackBool notBool  = Left $ TypeMismatch "bool" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsException Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  case res of -- turn the error into a False result
    Left _ -> return False
    x      -> x
  where
    res = do unpacked1 <- unpacker arg1
             unpacked2 <- unpacker arg2
             return $ unpacked1 == unpacked2

-- List functions
car :: [LispVal] -> ThrowsException LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [DottedList [] x]    = return x
car [badArg]             = Left $ TypeMismatch "pair" badArg
car badArgList           = Left $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsException LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = Left $ TypeMismatch "pair" badArg
cdr badArgList            = Left $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsException LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs]  = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]   = return $ DottedList [x1] x2
cons badArgList = Left $ NumArgs 2 badArgList

-- Equality testing
eqv :: [LispVal] -> ThrowsException LispVal
eqv [Bool arg1, Bool arg2]     = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]     = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2)
  && (and $ zipWith go arg1 arg2)
    where
      go x1 x2 = case eqv [x1, x2] of
        Left _ -> False
        Right (Bool val) -> val
        _ -> undefined
eqv [_, _]     = return $ Bool False
eqv badArgList = Left $ NumArgs 2 badArgList

-- Use weak typing in an attempt to get equality
equal :: [LispVal] -> ThrowsException LispVal
equal [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2)
  && (and $ zipWith go arg1 arg2)
    where
      go x1 x2 = case equal [x1, x2] of
        Left _ -> False
        Right (Bool val) -> val
        _ -> undefined
equal [arg1, arg2] = do
      primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = Left $ NumArgs 2 badArgList
