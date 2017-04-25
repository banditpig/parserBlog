{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Char 
import Control.Applicative hiding (many)

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p)  = p

ch :: Parser Char
ch = P (\s -> case s of
                    []     -> []
                    (x:xs) -> [(x, xs)])


-- empty list denotes failure, 
-- and a singleton list denotes success

-- That is, fmap applies a function to the 
-- result value of a parser if the parser succeeds, 
--     and propagates the failure otherwise.

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p  = P (\s -> case parse p s of
                             [] -> []
                             [(x, xs)] -> [(f x, xs)])

 
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\s -> [(x, s)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pab <*> pa = P (\s -> case parse pab s of
                    [] -> []
                    [(fab, res)] -> parse (fmap fab pa) res)



instance Monad Parser where
   -- return :: a -> Parser a
   return x = P (\s -> [(x, s)])
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   pa >>= f = P (\s -> case parse pa s of
                  [] -> []
                  [(a, res)] -> parse (f a) res)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> [])
    -- (<|>) :: Parser a-> Parser a -> Parser a   
    -- if p1 works the that one otherwise p2 
    p1 <|> p2 = P (\s -> case parse p1 s of
                        []  -> parse p2 s
                        res -> res)
-- First of all, we define a parser satisfy p for 
-- single characters that satisfyisfy the predicate p

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    x <- ch
    if p x then return x else P (\_ -> [])

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' p  = ch >>= (\x -> if p x then return x else P (\_ -> []))


-- parse a given char
char :: Char -> Parser Char
char x = satisfy (==x) 

-- couple of predicates
isVowel :: Char -> Bool 
isVowel c = elem (toLower c) ['a','e', 'i', 'o', 'u']

isConsonant :: Char -> Bool
isConsonant  = not . isVowel 

digit :: Parser Char
digit = satisfy isDigit

lowerCase :: Parser Char
lowerCase = satisfy isLower

upperCase :: Parser Char
upperCase = satisfy isUpper

alpha :: Parser Char
alpha = satisfy isAlpha

alphaNum  :: Parser Char
alphaNum  = satisfy isAlphaNum

vowel :: Parser Char
vowel = satisfy isVowel

consonant :: Parser Char
consonant = satisfy isConsonant

string :: String -> Parser String
string [] = return []
string s@(x:xs) = do
           char x
           string xs
           return s

string' :: String -> Parser String
string' [] = return []
string' s@(x:xs) = char x *> string' xs *> return s

httpVerb :: Parser String
httpVerb = string "GET" <|> string "POST"  <|> string "DELETE"

httpPreamble :: Parser String
httpPreamble = do
  verb <- httpVerb
  char ','
  return verb

-- or
httpPreamble' :: Parser String
httpPreamble' = httpVerb <* char ','


many :: Parser a -> Parser [a]   
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a] 
many1 p = pure (:) <*> p <*> many p

ident :: Parser String
ident = do
    x  <- lowerCase
    xs <- many alphaNum 
    return (x:xs) 

ident' :: Parser String
ident' = pure (:) <*> lowerCase <*> many alphaNum 
    
natural :: Parser Int 
natural = do
  xs <- many1 digit
  return (read xs)   

natural' :: Parser Int 
natural' = pure read <*> many1 digit

space :: Parser ()
space = do
    many (satisfy isSpace )
    return ()

space' :: Parser ()
space' = many (satisfy isSpace ) *> return ()

int :: Parser Int
int = do
    char '-'
    n <- natural
    return (-n)
    <|> natural

dbl :: Parser Double
dbl = do
       char '-'
       d <- dbl'
       return (-d)
      <|>
       dbl'

dbl' :: Parser Double
dbl' = do
  d1 <- manyUntil digit (char '.')
  d2 <- many1 digit
  return (read (d1 ++ "." ++ d2)::Double)

int' :: Parser Int
int' = char '-' *> pure ((-1)*) <*> natural <|> natural

dropSpaces :: Parser a -> Parser a
dropSpaces p = do 
    space 
    v <- p
    space
    return v

dropSpaces' :: Parser a -> Parser a
dropSpaces' p = space *> p <* space

identifier :: Parser String
identifier = dropSpaces ident

integer :: Parser Int
integer = dropSpaces int

literal :: String -> Parser String
literal xs = dropSpaces (string xs)

anyString'  :: Parser String
anyString' = do
  x <- alphaNum
  xs <- many alphaNum
  return (x:xs)

anyString :: Parser String
anyString = dropSpaces anyString'

comment :: String ->  Parser ()
comment cmnt = do
      string cmnt 
      many (satisfy ( /='\n') )
      return ()

comment' :: String ->  Parser ()
comment' cmnt = string cmnt *> many (satisfy (/='\n') ) *> return ()

blockComment :: String -> String -> Parser ()
blockComment stS endS= do
  string stS
  manyUntil ch (string endS)
  return ()

blockComment' :: String -> String ->  Parser () 
blockComment' stS endS = string stS >> manyUntil ch (string endS) >> return ()

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil p endp = go where
  go = do 
          endp
          return []
       <|>
         do
          x <- p
          xs <- go
          return (x:xs)

manyUntil' :: Parser a -> Parser b -> Parser [a]
manyUntil' p endp = scan' where 
  scan' = endp *> return [] <|> pure (:) <*> p <*> scan'
