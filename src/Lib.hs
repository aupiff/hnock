{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Function (on)
import           Data.List (sortBy)
import           Text.ParserCombinators.Parsec ( Parser
                                               , char
                                               , choice
                                               , digit
                                               , eof
                                               , letter
                                               , many1
                                               , oneOf
                                               , parse
                                               , spaces
                                               , string
                                               , try
                                               , (<|>)
                                               )
import Debug.Trace (traceShowId)


data Noun = Cell Noun Noun
          | Atom Integer
          deriving (Show, Eq)

-- PARSING

test = eval <$> parse noun "" "[4 1 3]"

testf x = eval <$> parse noun "" x
-- "" "  [ [1 0] 3 [4 0 1] 4 5 6 8     [1 0]   ]"

program :: Parser Noun
program = noun <* eof

stripSpaces p = spaces *> p <* spaces

noun :: Parser Noun
noun = stripSpaces (try atom <|> cell)


cell :: Parser Noun
cell = foldr1 Cell <$> (char '[' *> many1 noun <* char ']')


atom :: Parser Noun
atom = Atom . (read :: String -> Integer) <$>
    ((char '[' *> stripSpaces (many1 digit) <* char ']') <|> many1 digit)


-- EVALUATION

-- ?[a b]           0
-- ?a               1
-- +[a b]           +[a b]
-- +a               1 + a
-- =[a a]           0
-- =[a b]           1
-- =a               =a
--
-- /[1 a]           a
-- /[2 a b]         a
-- /[3 a b]         b
-- /[(a + a) b]     /[2 /[a b]]
-- /[(a + a + 1) b] /[3 /[a b]]
-- /a               /a
--
-- *[a [b c] d]     [*[a b c] *[a d]]
--
-- *[a 0 b]         /[b a]
-- *[a 1 b]         b
-- *[a 2 b c]       *[*[a b] *[a c]]
-- *[a 3 b]         ?*[a b]
-- *[a 4 b]         +*[a b]
-- *[a 5 b]         =*[a b]
--
-- *[a 6 b c d]     *[a 2 [0 1] 2 [1 c d] [1 0]
--                    2 [1 2 3] [1 0] 4 4 b]
-- *[a 7 b c]       *[a 2 b 1 c]
-- *[a 8 b c]       *[a 7 [[7 [0 1] b] 0 1] c]
-- *[a 9 b c]       *[a 7 c 2 [0 1] 0 b]
-- *[a 10 [b c] d]  *[a 8 c 7 [0 3] d]
-- *[a 10 b c]      *[a c]
--
-- *a               *a

eval :: Noun -> Noun
eval (Cell a (Cell (Atom 0) (Atom b))) = treeLookup b a
eval (Cell a (Cell (Atom 1) b)) = b -- 1, K combinator
eval (Cell a (Cell (Atom 2) (Cell b c))) = eval (Cell (eval (Cell a b)) (eval (Cell a c)))
eval (Cell a (Cell (Atom 3) b)) = case eval (Cell a b) of
                                    Atom _ -> Atom 0
                                    Cell _ _ -> Atom 1
eval (Cell a (Cell (Atom 4) b)) = case eval (Cell a b) of
                                    Atom v -> Atom (v + 1)
                                    _      -> undefined
eval (Cell a (Cell (Atom 5) b)) = if a == b then Atom 0 else Atom 1

treeLookup :: Integer -> Noun -> Noun
treeLookup 1 n = n
treeLookup 2 (Cell a b) = a
treeLookup 3 (Cell a b) = b
treeLookup a b = if even a then treeLookup 2 (treeLookup (div a 2) b)
                           else treeLookup 3 (treeLookup (div (a - 1) 2) b)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
