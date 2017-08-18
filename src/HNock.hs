{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HNock where

import           Control.Monad
import           Data.Either.Combinators (mapLeft)
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
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

data Noun = !Noun :-: !Noun | Atom !Integer deriving Eq

instance Show Noun where
    show (Atom x) = show x
    show (a :-: b) = "[" ++ show a ++ " " ++ show b ++ "]"

infixr 7 :-:

{-- PARSING --}

parseEvalNoun = eval <=< parseNoun
parseNoun = mapLeft show . parse noun ""

program :: Parser Noun
program = noun <* eof

stripSpaces p = spaces *> p <* spaces

noun :: Parser Noun
noun = stripSpaces (try atom <|> cell)


cell :: Parser Noun
cell = foldr1 (:-:) <$> (char '[' *> many1 noun <* char ']')


atom :: Parser Noun
atom = Atom . (read :: String -> Integer) <$>
    ((char '[' *> stripSpaces (many1 digit) <* char ']') <|> many1 digit)


{-- EVALUATION --}

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

eval :: Noun -> Either String Noun
eval (a :-: (b :-: c) :-: d) = do r1 <- eval (a :-: b :-: c)
                                  r2 <- eval (a :-: d)
                                  return $ r1 :-: r2
eval x@(a :-: Atom 0 :-: Atom b) = treeLookup b a
eval (a :-: Atom 1 :-: b) = Right b
eval (a :-: Atom 2 :-: b :-: c) = do r1 <- eval (a :-: b)
                                     r2 <- eval (a :-: c)
                                     eval (r1 :-: r2)
eval (a :-: Atom 3 :-: b) = eval (a :-: b) >>= (\x -> case x of
                                                   Atom _ -> Right $ Atom 1
                                                   (:-:) _ _ -> Right $ Atom 0)
eval y@(a :-: Atom 4 :-: b) = eval (a :-: b) >>= (\x -> case x of
                        Atom v -> Right $ Atom (v + 1)
                        _      -> Left ("incrementing non-atom: " ++ show y))
eval (a :-: Atom 5 :-: b) = Right $ if a == b then Atom 0 else Atom 1
-- eval (a :-: Atom 6 :-: b :-: c :-: d) = eval $
--      a :-: Atom 2 :-: (Atom 0 :-: Atom 1) :-: Atom 2 :-: (Atom 1 :-: c :-: d)
--        :-: (Atom 1 :-: Atom 0) :-: Atom 2 :-: (Atom 1 :-: Atom 2 :-: Atom 3)
--        :-: (Atom 1 :-: Atom 0) :-: Atom 4 :-: Atom 4 :-: b
-- eval (a :-: Atom 7 :-: b :-: c) = eval $ a :-: Atom 2 :-: b :-: Atom 1 :-: c
-- eval (a :-: Atom 8 :-: b :-: c) = eval $
--     a :-: Atom 7 :-: ((Atom 7 :-: (Atom 0 :-: Atom 1) :-: b) :-: Atom 0 :-: Atom 1) :-: c
-- eval (a :-: Atom 9 :-: b :-: c) = eval $
--     a :-: Atom 7 :-: c :-: Atom 2 :-: (Atom 0 :-: Atom 1) :-: Atom 0 :-: b
-- eval (a :-: Atom 10 :-: (b :-: c) :-: d) = eval $
--     a :-: Atom 8 :-: c :-: Atom 7 :-: (Atom 0 :-: Atom 3) :-: d
-- eval (a :-: Atom 10 :-: b :-: c) = eval $ a :-: c
eval x = Left (show x)

treeLookup :: Integer -> Noun -> Either String Noun
treeLookup 1 n = Right n
treeLookup 2 (a :-: b) = Right a
treeLookup 3 (a :-: b) = Right b
treeLookup a b@(_ :-: _)
    | a <= 0 = Left "failed treeLookup"
    | even a = treeLookup 2 =<< treeLookup (div a 2) b
    | otherwise = treeLookup 3 =<< treeLookup (div (a - 1) 2) b
treeLookup _ _ = Left "failed treeLookup"
