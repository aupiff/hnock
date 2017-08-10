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
          deriving Show

test = parse noun "" "[[1 0] 3 [4 0 1] 4 5 6 8     [1 0]]"
-- [4 0 1] 3 4 4 4]"

program :: Parser Noun
program = noun <* eof


noun :: Parser Noun
noun = spaces *> (try atom <|> cell) <* spaces


cell :: Parser Noun
cell = foldr1 Cell <$> (char '[' *> many1 noun <* char ']')


atom :: Parser Noun
atom = Atom . (read :: String -> Integer) <$>
            ((char '[' *> spaces *> many1 digit <* spaces <* char ']')
         <|> many1 digit)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
