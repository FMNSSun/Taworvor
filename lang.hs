import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import Data.Maybe
import qualified Data.Map as M
import Data.Char
import Data.Bits
import Numeric
import Data.List

data Value = IntVal Int | DoubleVal Double | ListVal [Value] | Func [Expression] | Mark
 deriving (Eq, Ord)

instance Show Value where
  show (IntVal a) = show a
  show (DoubleVal a) = showFFloat (Just 16) a ""
  show (ListVal a) = "[" ++ intercalate " " (map show a) ++ "]"

instance Num Value where
  (IntVal a) * (IntVal b) = IntVal $ a * b
  (DoubleVal a) * (DoubleVal b) = DoubleVal $ a * b

  (ListVal a) + (ListVal b) = ListVal $ a ++ b

  (IntVal a) + (IntVal b) = IntVal $ a + b
  (DoubleVal a) + (DoubleVal b) = DoubleVal $ a + b

  negate (IntVal a) = IntVal $ -a
  negate (DoubleVal a) = DoubleVal $ -a

  abs (IntVal a) = IntVal $ abs a
  abs (DoubleVal a) = DoubleVal $ abs a
  signum = error "signum"
  fromInteger q = IntVal (fromIntegral q)

instance Fractional Value where
  (IntVal a) / (IntVal b) = IntVal $ a `div` b
  (DoubleVal a) / (DoubleVal b) = DoubleVal $ a  / b
  fromRational = undefined

data Expression = If [Expression] [Expression] | 
                  Value' Value       | 
                  Operator Char      | 
                  Load String        |
                  Store String       |
                  Call String        |
                  String' String     |
                  Require String
 deriving (Eq, Ord, Show)

parseName :: Parser String
parseName = many $ oneOf ('.' : ['A'..'Z'])

parseProgram :: Parser (M.Map String [Expression])
parseProgram = do
  string "PROGRAM"
  optional spaces
  string "^AUTHOR"
  many $ noneOf "^"
  optional spaces
  string "^NAME"
  many $ noneOf "^"
  optional spaces
  string "^DESC"
  many $ noneOf "^"
  optional spaces
  string $ "^IS"
  optional spaces
  prog <- many1 $ parseProcedure
  string "END"
  optional spaces
  return (M.fromList prog)

parseLibrary :: Parser (M.Map String [Expression])
parseLibrary = do
  string "LIBRARY"
  optional spaces
  string "^AUTHOR"
  many $ noneOf "^"
  optional spaces
  string "^NAME"
  many $ noneOf "^"
  optional spaces
  string "^DESC"
  many $ noneOf "^"
  optional spaces
  string $ "^IS"
  optional spaces
  lib <- many1 $ parseProcedure
  string "END"
  optional spaces
  return (M.fromList lib)

parseProcedure :: Parser (String, [Expression])
parseProcedure = do
  string "PROCEDURE"
  optional spaces
  name <- parseName
  optional spaces
  code <- many $ parseExpression
  optional spaces
  string "END"
  optional spaces
  return (name, code)

parseExpression :: Parser Expression
parseExpression = 
 do optional (try parseComment)
    exp <- (do { val <- parseValue; return $ Value' val; }) <|> parseOperator <|> parseLoad <|> parseStore <|> parseCall <|> parseFastCall <|> parseIf <|> parseRequire
    optional (try parseComment)
    return exp

parseValue = do
  optional (try parseComment)
  val <- (try parseLiteralInt) <|> (try parseLiteralDouble) <|> parseString <|> parseFunc
  optional (try parseComment)
  return val

parseFunc :: Parser Value
parseFunc = do
  string "FUNC"
  optional spaces
  code <- many $ parseExpression
  optional spaces
  string "END"
  optional spaces
  return $ Func code

parseString :: Parser Value
parseString = do
  char '{'
  txt <- many1 $ noneOf "}"
  char '}'
  optional spaces
  return $ (ListVal (map (IntVal . ord) $ txt))

parseComment :: Parser Expression
parseComment = do
  string "REM"
  optional spaces
  many $ noneOf "^"
  string "^END"
  optional spaces
  return $ String' []

parseLiteralDouble :: Parser Value
parseLiteralDouble = do
  char 'D'
  sign <- optionMaybe $ char '-'
  digits <- many1 $ oneOf ['0'..'9']
  char '.'
  digits1 <- many1 $ oneOf ['0'..'9']
  optional spaces
  return $ 
    (DoubleVal (read $ (fromMaybe '0' sign) : (digits ++ "." ++ digits1)))

parseLiteralInt :: Parser Value
parseLiteralInt = do
  sign <- optionMaybe $ char '-'
  digits <- many1 $ oneOf ['0'..'9']
  optional spaces
  return  $
    (IntVal (read $ (fromMaybe '0' sign) : digits))

parseOperator :: Parser Expression
parseOperator = do
  ch <- oneOf "+-/*%@#$&|~`=<>\"\\:?'^_(!),;[]"
  optional spaces
  return (Operator ch)

parseFastCall :: Parser Expression
parseFastCall = do
  ch <- many1 $ oneOf ['a'..'z']
  let ch' = map toUpper ch
  return $ Call ch'

parseRequire :: Parser Expression
parseRequire = do
  string "REQUIRE"
  optional spaces
  path <- many1 $ noneOf " \t\n^"
  optional spaces
  string "^OK"
  optional spaces
  return (Require path)

parseLoad :: Parser Expression
parseLoad = do
  string "LOAD"
  optional spaces
  name <- parseName
  optional spaces
  return (Load name)

parseStore :: Parser Expression
parseStore = do
  string "STORE"
  optional spaces
  name <- parseName
  optional spaces
  return (Store name)

parseCall :: Parser Expression
parseCall = do
  string "CALL"
  optional spaces
  name <- parseName
  optional spaces
  return (Call name)

parseIf :: Parser Expression
parseIf = do
  string "IF"
  optional spaces
  code_t <- many $ parseExpression
  optional spaces
  string "ELSE"
  optional spaces
  code_f <- many $ parseExpression
  optional spaces
  string "END"
  optional spaces
  return (If code_t code_f)

parseList :: Parser Value
parseList = do
  char '['
  optional spaces
  elems <- many $ (parseValue <|> parseList)
  optional spaces
  char ']'
  optional spaces
  return $ (ListVal elems)

runParserWithString p input = 
  case parse p "" input of
    Left err -> error $ show err
    Right q -> q

parseAndEval input = do
  let prog = runParserWithString parseProgram input
  call "MAIN" (prog, [])

runFile path = do
  txt <- readFile path
  parseAndEval txt
  return ()

eval :: [Expression] -> (M.Map String [Expression], [Value]) -> IO (M.Map String [Expression], [Value])
eval ((Require path):xs) (things, stack) = do
  txt <- readFile path
  let lib = runParserWithString parseLibrary txt
  eval xs (M.union lib things, stack)
eval (x:xs) state = eval' x state >>= eval xs
eval [] state = return state

eval' :: Expression -> (M.Map String [Expression], [Value]) -> IO (M.Map String [Expression], [Value])
eval' (Value' i) (things, stack) = return (things, i : stack)
eval' (Load q) state = load q state
eval' (Store q) state = store q state
eval' (Call q) state = call q state
eval' (If t f) (things, (1 : xs)) = eval t (things, xs)
eval' (If t f) (things, (_ : xs)) = eval f (things, xs)
eval' (String' s) (things, xs) = return (things, (map (IntVal . ord) s) ++ xs)
eval' (Operator c) state = evalOperator c state
eval' q a = error $ (show q) ++ " <_> " ++ (show a)

call :: String -> (M.Map String [Expression], [Value]) -> IO (M.Map String [Expression], [Value])
call name state@(things, stack) = do
  case M.lookup name things of
    Nothing -> error $ "Unknown function: " ++ name
    Just q -> eval q state

load :: String -> (M.Map String [Expression], [Value]) -> IO (M.Map String [Expression], [Value])
load name state@(things, stack) = do
  case M.lookup name things of
    Just [(Value' q)] -> return (things, q : stack)
    _ -> error $ "Can not load: " ++ name

store :: String -> (M.Map String [Expression], [Value]) -> IO (M.Map String [Expression], [Value])
store name state@(things, stack) = do
  case stack of
    (i : xs) -> return (M.insert name [Value' i] things, xs)
    _ -> error $ "Can not store: " ++ name

evalOperator '+' (things, (b : a : xs)) = return (things, (a+b : xs))
evalOperator '-' (things, (b : a : xs)) = return (things, (a-b : xs))
evalOperator '*' (things, (b : a : xs)) = return (things, (a*b : xs))
evalOperator '*' (things, ((ListVal b) :xs)) = return (things, ((DoubleVal $ read (stringify b))) : xs)
evalOperator '/' (things, (b : a : xs)) = return (things, (a/b : xs))
evalOperator '/' (things, ((ListVal b) :xs)) = return (things, ((runParserWithString (parseList <|> parseValue) (stringify b))) : xs)
evalOperator '%' (things, ((IntVal b) : (IntVal a) : xs)) = return (things, ((IntVal $ a`mod`b) : xs))
evalOperator '%' (things, ((ListVal b) : xs)) = return (things, ((ListVal $ init b) : xs))
evalOperator '@' (things, (b : xs)) = print b >> return (things, xs)
evalOperator '#' (things, ((IntVal b) : xs)) = putChar (chr b) >> return (things, xs)
evalOperator '$' (things, ((IntVal b) : (IntVal a) : xs)) = return (things, ((IntVal $ a`xor`b) : xs))
evalOperator '&' (things, ((IntVal b) : (IntVal a) : xs)) = return (things, ((IntVal $ (a .&. b)) : xs))
evalOperator '|' (things, ((IntVal b) : (IntVal a) : xs)) = return (things, ((IntVal $ (a .|. b)) : xs))
evalOperator '~' (things, ((IntVal b) : xs)) = return (things, ((IntVal $ complement b) : xs))
evalOperator '~' (things, ((ListVal b) : xs)) = return (things, ((IntVal $ length b) : xs))
evalOperator '`' (things, xs) = getChar >>= (\x -> return (things, (IntVal (ord x)) : xs))
evalOperator ',' (things, xs) = getContents >>= (\x -> return (things, (ListVal $ map (IntVal . ord) x) : xs))
evalOperator ';' (things, (b : a : xs))
 |b == a = return (things, xs)
 |otherwise = error $ "FAIL: " ++ (show a) ++ "," ++ (show b) ++ "," ++ (show xs)
evalOperator '=' (things, (b : a : xs))
 |b == a = return (things, 1 : xs)
 |otherwise = return (things, 0 : xs)
evalOperator '>' (things, (b : a : xs))
 |a > b = return (things, 1 : xs)
 |otherwise = return (things, 0 : xs)
evalOperator '<' (things, (b : a : xs))
 |a < b = return (things, 1 : xs)
 |otherwise = return (things, 0 : xs)
evalOperator '"' (things, b : xs) = return (things, b : b : xs)
evalOperator '\\' (things, b : a : xs) = return (things, a : b : xs)
evalOperator ':' (things, b : xs) = return (things, ((ListVal $ map (IntVal . ord) $ show b) : xs))
evalOperator '?' (things, []) = return (things, [0])
evalOperator '?' (things, xs) = return (things, 1 : xs)
evalOperator '\'' (things, (b : xs)) = return (things, xs)
evalOperator '^' (things, ((DoubleVal b) : xs)) = return (things, ((IntVal $ ceiling b) : xs))
evalOperator '^' (things, ((ListVal b) : xs)) = return (things, ((head b) : xs))
evalOperator '_' (things, ((DoubleVal b) : xs)) = return (things, ((IntVal $ floor b) : xs))
evalOperator '_' (things, ((IntVal b) : xs)) = return (things, ((DoubleVal $ (fromIntegral b)) : xs))
evalOperator '_' (things, ((ListVal b) : xs)) = return (things, ((ListVal $ tail b) : xs))
evalOperator '(' (things, (b : xs)) = return (things, ((ListVal [b]) : xs))
evalOperator '!' (things, ((IntVal b):(ListVal a):xs)) = return (things, (a !! b) : xs)
evalOperator ')' (things, ((IntVal _) : xs)) = return (things, ((IntVal 1) : xs))
evalOperator ')' (things, ((DoubleVal _) : xs)) = return (things, ((IntVal 2) : xs))
evalOperator ')' (things, ((ListVal _) : xs)) = return (things, ((IntVal 3) : xs))
evalOperator ')' (things, ((Func _) : xs)) = return (things, ((IntVal 4) : xs))
evalOperator ')' (things, ((Mark) : xs)) = return (things, ((IntVal 5) : xs))
evalOperator '!' (things, ((Func b) : xs)) = do
  (things', stack') <- eval b (things, xs)
  return (things', stack')
evalOperator '[' (things, xs) = return (things, Mark : xs)
evalOperator ']' (things, xs) = do
  let t = takeWhile (/= Mark) xs
  let t' = dropWhile (/= Mark) xs
  return (things, (ListVal (reverse t)) : (tail t'))

-- All hope is lost
evalOperator q a = error $ (show q) ++ " <_> " ++ (show a)

stringify b = concatMap (\c -> case c of 
                             IntVal a -> [chr a]
                             _ -> "") b
unstringify b = map (IntVal . ord) b
