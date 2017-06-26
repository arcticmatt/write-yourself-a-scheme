{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import System.IO
import System.Environment
import Control.Monad (void)
-- import Control.Exception
import Control.Monad.Except
import Data.IORef

import Text.Megaparsec as TM
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-- *** REPL Stuff ***
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

-- Prints string then flushes stdout
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Prints prompt, reads input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Parses, evaluates, handles errors
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ (liftThrows $ readExpr expr) >>= eval env

-- Evals and prints the result
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- The underscore after the name is a typical naming convention in Haskell for
-- monadic functions that repeat but do not return a value
-- predicate: signals when to stop
-- prompt: action to perform before the test
-- action: function-returning-an-action to do to the input
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

-- Takes the name of a file to execute and runs that as a program. Additional
-- command-line arguments get bound into a list args within the Scheme program.
runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
-- runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint


-- *** Data Structures & Types ***
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String  -- Constructors and types have different namespaces!
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsException LispVal)
             | Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsException LispVal)
             | Port Handle

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name)       = name
  show (Number contents) = show contents
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (List contents)   = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t)  = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func {params = args, vararg = varargs, body = _, closure = _}) =
    "(lambda (" ++ unwords (map show args) ++
      (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
  show (IOFunc _) = "<IO primitive>"
  show (Port _)   = "<IO port>"

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

trapException :: (Show a, MonadError a m) => m String -> m String
trapException action = catchError action (return . show)

extractValue :: ThrowsException a -> a
extractValue (Right val) = val
extractValue _ = error "extractValue error"

type IOThrowsException = ExceptT LispException IO

liftThrows :: ThrowsException a -> IOThrowsException a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsException String -> IO String
runIOThrows action = runExceptT (trapException action) >>= return . extractValue

makeFunc :: (Show a, Monad m) => Maybe String -> Env -> [a] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispException IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispException IO LispVal
makeVarArgs = makeFunc . Just . show

-- Uses existential quantification
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsException a)

-- Two ways the program can mutate the environment.
-- 1) set! a variable
-- 2) define a variable
type Env = IORef [(String, IORef LispVal)]

primitives :: [(String, [LispVal] -> ThrowsException LispVal)]
primitives =
  [ ("+", numericBinop (+))
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

ioPrimitives :: [(String, [LispVal] -> IOThrowsException LispVal)]
ioPrimitives =
  [ ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
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

readOrThrow :: Parser a -> String -> ThrowsException a
readOrThrow parser input = case parse parser "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsException LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsException [LispVal]
readExprList = readOrThrow (endBy parseExpr sc)

-- *** Evaluation ***
eval :: Env -> LispVal -> IOThrowsException LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _)   = return val
eval env (Atom id_) = getVar env id_
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) =
  do result <- eval env predicate
     case result of
       Bool False -> eval env alt
       Bool True  -> eval env conseq
       x          -> throwError $ TypeMismatch "bool" x
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
-- Need to implement load here because apply can't modify an environment.
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsException LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = fmap last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args

applyProc :: [LispVal] -> IOThrowsException LispVal
applyProc [func, List args] = apply func args
applyProc (func:args)       = apply func args

-- Takes a primitive Haskell function (often an operator section) and wraps it
-- with code to unpack an argument list, apply the function to it, and
-- wrap the result up in our Number constructor.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsException LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ [singleVal] = throwError $ NumArgs 2 [singleVal]
numericBinop op params_ = Number . foldl1 op <$> mapM unpackNum params_

boolBinop :: (LispVal -> ThrowsException a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsException LispVal
boolBinop _ _ args | length args /= 2 = throwError $ NumArgs 2 args
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
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- Weak typing
unpackStr :: LispVal -> ThrowsException String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsException Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "bool" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsException Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

-- List functions
car :: [LispVal] -> ThrowsException LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [DottedList [] x]    = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgList           = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsException LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

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
eqv badArgList = throwError $ NumArgs 2 badArgList

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
equal badArgList = throwError $ NumArgs 2 badArgList

-- *** Environment stuff ***
nullEnv :: IO Env
nullEnv = newIORef []

-- Binds primitives upon startup.
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

-- Determines if a given variable is already bound in the environment.
-- Necessary for define.
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- Retrieves the current value of a variable.
getVar :: Env -> String -> IOThrowsException LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsException LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value)) (lookup var env)
  return value -- return value we just set for convenience

-- define a variable.
-- Either sets an existing variable or defines a new one.
defineVar :: Env -> String -> LispVal -> IOThrowsException LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value  -- set existing variable
    else liftIO $ do              -- define new variable
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

-- Bind multiple variables at once.
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings' env = fmap (++ env) (mapM addBinding bindings')
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)


-- *** IO Ports and Stuff ***
-- Wraps the Haskell function openFile, converting it to the right type and
-- wrapping its return value in the Port constructor. It's intended to be
-- partially-applied to the IOMode, ReadMode for open-input-file and WriteMode
-- for open-output-file.
makePort :: IOMode -> [LispVal] -> IOThrowsException LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

-- closePort wraps the equivalent Haskell procedure, hClose.
closePort :: [LispVal] -> IOThrowsException LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

-- readProc wraps the Haskell hGetLine and then send the result to parseExpr, to
-- be turned into a LispVal suitable for Scheme.
readProc :: [LispVal] -> IOThrowsException LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

-- writeProc converts a LispVal to a string and then writes it out on the specified
-- port.
writeProc :: [LispVal] -> IOThrowsException LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

-- Reads the whole file into a string in memory. It's a thin wrapper around Haskell's
-- readFile, again just lifting the IO action into an IOThrowsException action
-- and wrapping it in a String constructor.
readContents :: [LispVal] -> IOThrowsException LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

-- Doesn't do what Scheme's load does. Rather, it's responsible only for reading
-- and parsing a file full of statements. It's used in two places: readAll (which
-- returns a list of values) and load (which evaluates those values as Scheme
-- expressions).
-- For actual load functionality, see eval.
load :: String -> IOThrowsException [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- Wraps value returned by load with the List constructor.
readAll :: [LispVal] -> IOThrowsException LispVal
readAll [String filename] = fmap List $ load filename
