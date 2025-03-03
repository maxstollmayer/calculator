module Lib where

import Control.Applicative (Alternative (empty, many, some, (<|>)), optional)
import Data.Char (isDigit, isSpace)
import Data.Functor (void, ($>), (<$))

data Expr
  = Num Double
  | Sqrt Expr
  | Log Expr
  | Sin Expr
  | Cos Expr
  | Tan Expr
  | Asin Expr
  | Acos Expr
  | Atan Expr
  | Sinh Expr
  | Cosh Expr
  | Tanh Expr
  | Asinh Expr
  | Acosh Expr
  | Atanh Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  deriving (Show)

-- TODO: use minimal parentheses
pprint :: Expr -> String
pprint (Num num) = show num
pprint (Sqrt x) = pprintUnOp "sqrt" x
pprint (Log x) = pprintUnOp "log" x
pprint (Sin x) = pprintUnOp "sin" x
pprint (Cos x) = pprintUnOp "cos" x
pprint (Tan x) = pprintUnOp "tan" x
pprint (Asin x) = pprintUnOp "asin" x
pprint (Acos x) = pprintUnOp "acos" x
pprint (Atan x) = pprintUnOp "atan" x
pprint (Sinh x) = pprintUnOp "sinh" x
pprint (Cosh x) = pprintUnOp "cosh" x
pprint (Tanh x) = pprintUnOp "tanh" x
pprint (Asinh x) = pprintUnOp "asinh" x
pprint (Acosh x) = pprintUnOp "acosh" x
pprint (Atanh x) = pprintUnOp "atanh" x
pprint (Add x y) = pprintBinOp "+" x y
pprint (Sub x y) = pprintBinOp "-" x y
pprint (Mul x y) = pprintBinOp "*" x y
pprint (Div x y) = pprintBinOp "/" x y
pprint (Exp x y) = pprintBinOp "^" x y

pprintUnOp :: String -> Expr -> String
pprintUnOp op x = op ++ "(" ++ pprint x ++ ")"

pprintBinOp :: String -> Expr -> Expr -> String
pprintBinOp op x y = "(" ++ pprint x ++ " " ++ op ++ " " ++ pprint y ++ ")"

eval :: Expr -> Either String Double
eval (Num num) = Right num
eval (Sqrt x) = evalSafe "Square root of negative number." sqrt x
eval (Log x) = evalSafe "Logarithm of non-positive number." log x
eval (Sin x) = sin <$> eval x
eval (Cos x) = cos <$> eval x
eval (Tan x) = tan <$> eval x -- does not return NaN because of float errors
eval (Asin x) = evalSafe "Inverse sine of number outside [-1,1]." asin x
eval (Acos x) = evalSafe "Inverse cosine of number outside [-1,1]." acos x
eval (Atan x) = atan <$> eval x
eval (Sinh x) = sinh <$> eval x
eval (Cosh x) = cosh <$> eval x
eval (Tanh x) = tanh <$> eval x
eval (Asinh x) = asinh <$> eval x
eval (Acosh x) = evalSafe "Inverse hyperbolic cosine of negative number." acosh x
eval (Atanh x) = evalSafe "Inverse hyperbolic tangent of number outside (-1,1)." atanh x
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x y) = do
  val <- eval y
  if val == 0
    then Left "Division by zero."
    else (/ val) <$> eval x
eval (Exp x y) = do
  valx <- eval x
  valy <- eval y
  let res = valx ** valy
  if isNaN res
    then Left "Exponentiation of negative base."
    else Right res

evalSafe :: String -> (Double -> Double) -> Expr -> Either String Double
evalSafe msg f x = do
  val <- eval x
  let res = f val
  if isNaN res || isInfinite res
    then Left msg
    else Right res

-- Parser Interface

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  f `fmap` (Parser p) = Parser $ \input -> do
    (input', a) <- p input
    return (input', f a)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (Parser pf) <*> (Parser pa) = Parser $ \input -> do
    (input', f) <- pf input
    (input'', a) <- pa input'
    return (input'', f a)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (input', a) <- p input
    runParser (f a) input'

instance Alternative Parser where
  empty = Parser . const $ Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Nothing -> p2 input
    a -> a

-- Parser Combinators

sepBySome :: Parser a -> Parser sep -> Parser [a]
sepBySome p sep = (:) <$> p <*> many (sep *> p)

sepByMany :: Parser a -> Parser sep -> Parser [a]
sepByMany p sep = sepBySome p sep <|> pure []

chainLeft :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeft p op = p >>= rest
  where
    rest x = (op <*> pure x <*> p >>= rest) <|> pure x

chainRight :: Parser a -> Parser (a -> a -> a) -> Parser a
chainRight p op = p >>= \x -> (op <*> pure x <*> chainRight p op) <|> pure x

-- Basic Parsers

eof :: Parser ()
eof = Parser $ \input -> if null input then Just ("", ()) else Nothing

anyChar :: Parser Char
anyChar = Parser p
  where
    p "" = Nothing
    p (x : rest) = Just (rest, x)

char :: Char -> Parser Char
char c = Parser p
  where
    p "" = Nothing
    p (x : rest)
      | x == c = Just (rest, c)
      | otherwise = Nothing

string :: String -> Parser String
string = traverse char

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser f
  where
    f "" = Nothing
    f (x : xs) = if pred x then Just (xs, x) else Nothing

oneOf :: String -> Parser Char
oneOf elems = satisfy (`elem` elems)

noneOf :: String -> Parser Char
noneOf elems = satisfy (`notElem` elems)

whitespace :: Parser ()
whitespace = void $ many $ satisfy isSpace

digit :: Parser Char
digit = satisfy isDigit

lexeme :: Parser a -> Parser a
lexeme p = whitespace *> p <* whitespace

symbol :: String -> Parser ()
symbol = void . lexeme . string

-- Expr Parsers

number :: Parser Expr
number = Num . read <$> lexeme num
  where
    num = do
      sign <- string "-" <|> pure ""
      int <- some digit
      dec <- (char '.' *> some digit) <|> pure "0"
      return $ sign ++ int ++ "." ++ dec

parens :: Parser Expr
parens = symbol "(" *> term <* symbol ")"

unary :: (Expr -> Expr) -> String -> Parser Expr
unary op sym = op <$> (symbol sym *> term)

atom :: Parser Expr
atom =
  number
    <|> unary Sqrt "sqrt"
    <|> unary Log "log"
    <|> unary Sin "sin"
    <|> unary Cos "cos"
    <|> unary Tan "tan"
    <|> unary Asin "asin"
    <|> unary Acos "acos"
    <|> unary Atan "atan"
    <|> unary Sinh "sinh"
    <|> unary Cosh "cosh"
    <|> unary Tanh "tanh"
    <|> unary Asinh "asinh"
    <|> unary Acosh "acosh"
    <|> unary Atanh "atanh"
    <|> parens

expP :: Parser Expr
expP = chainRight atom (Exp <$ symbol "**")

mulDiv :: Parser Expr
mulDiv = chainLeft expP ((Mul <$ symbol "*") <|> (Div <$ symbol "/"))

addSub :: Parser Expr
addSub = chainLeft mulDiv ((Add <$ symbol "+") <|> (Sub <$ symbol "-"))

term :: Parser Expr
term = lexeme addSub

expr :: Parser Expr
expr = term <* eof

parse :: String -> Maybe Expr
parse input = do
  (_, t) <- runParser term input
  return t

calculate :: String -> Either String Double
calculate input = case parse input of
  Nothing -> Left "ERROR: Failed to parse expression."
  Just t -> case eval t of
    Left err -> Left $ "ERROR: " ++ err
    Right res -> Right res
