module Lib where

import Control.Applicative (Alternative (empty, many, some, (<|>)), optional)
import Data.Char (isDigit, isSpace)
import Data.Functor (void, ($>), (<$))

data Term
  = Num Double
  | Add Term Term
  | Sub Term Term
  | Mul Term Term
  | Div Term Term
  deriving (Show)

-- TODO: use minimal parentheses
pprint :: Term -> String
pprint (Num num) = show num
pprint (Add x y) = pprintBinOp "+" x y
pprint (Sub x y) = pprintBinOp "-" x y
pprint (Mul x y) = pprintBinOp "*" x y
pprint (Div x y) = pprintBinOp "/" x y

pprintBinOp :: String -> Term -> Term -> String
pprintBinOp op a b = "(" ++ pprint a ++ " " ++ op ++ " " ++ pprint b ++ ")"

eval :: Term -> Either String Double
eval (Num num) = Right num
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x y) = do
  denominator <- eval y
  if denominator == 0
    then Left "Division by zero."
    else (/ denominator) <$> eval x

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

symbol :: Char -> Parser ()
symbol = void . lexeme . char

-- Term Parsers

number :: Parser Term
number = Num . read <$> lexeme num
  where
    num = do
      sign <- string "-" <|> pure ""
      int <- some digit
      dec <- (char '.' *> some digit) <|> pure "0"
      return $ sign ++ int ++ "." ++ dec

parens :: Parser Term
parens = symbol '(' *> term <* symbol ')'

atom :: Parser Term
atom = number <|> parens

mulDiv :: Parser Term
mulDiv = chainLeft atom ((Mul <$ symbol '*') <|> (Div <$ symbol '/'))

addSub :: Parser Term
addSub = chainLeft mulDiv ((Add <$ symbol '+') <|> (Sub <$ symbol '-'))

term :: Parser Term
term = lexeme addSub <* eof

parse :: String -> Maybe Term
parse input = do
  (_, t) <- runParser term input
  return t

calculate :: String -> Either String Double
calculate input = case parse input of
  Nothing -> Left "ERROR: Failed to parse expression."
  Just t -> case eval t of
    Left err -> Left $ "ERROR: " ++ err
    Right res -> Right res
