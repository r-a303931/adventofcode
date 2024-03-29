module Aoclibs.Parser (Parser(..), item, charP, stringP, spanP, parseIf, ws, sepBy, intP, tokenP, wordP, charMapP, linesP) where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (rest, s') <- p s
    return (f rest, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x,s)
  (Parser p1) <*> (Parser p2) = Parser $ \s -> do
    (f, s') <- p1 s
    (a, s'') <- p2 s'
    return (f a, s'')

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
  (Parser p1) >>= f = Parser $ \s -> do
    (v, s') <- p1 s
    parse (f v) s'

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> Nothing
    (c:cs) -> Just (c,cs)

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) = if x == c then Just (x, xs) else Nothing
        f []     = Nothing

stringP     :: String -> Parser String
stringP str = Parser $ parse (traverse charP str)

spanP :: (Char -> Bool) -> Parser String
spanP = many . parseIf

parseIf   :: (Char -> Bool) -> Parser Char
parseIf f =
  Parser $ \s ->
    case s of
      (y:ys) | f y -> Just (y,ys)
      _ -> Nothing

ws :: Parser String
ws = spanP isSpace

sepBy             :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element)


linesP    :: Parser a -> Parser [a]
linesP el = many (el <* charP '\n')

intP :: Parser Int
intP = read <$> (some . parseIf) isDigit

tokenP :: Parser String
tokenP = spanP (not . isSpace)

wordP :: Parser String
wordP = spanP isAlpha

charMapP :: [(Char, a)] -> Parser a
charMapP = asum . fmap (\(c,v) -> const v <$> charP c)
