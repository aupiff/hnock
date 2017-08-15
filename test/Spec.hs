{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
    describe "tree lookup" $ do

        it "*[5 0 1] -> 5" $
            eval (Atom 5 :-: Atom 0 :-: Atom 1)
                `shouldBe` Atom 5

        it "*[[5 1] 0 1] -> [5 1]" $
            eval ((Atom 5 :-: Atom 1) :-: Atom 0 :-: Atom 1)
                `shouldBe` (Atom 5 :-: Atom 1)

        it "*[[5 1] 0 2] -> 5" $
                eval ((Atom 5 :-: Atom 1) :-: Atom 0 :-: Atom 2)
                    `shouldBe` Atom 5

        it "*[[5 1] 0 3] -> 1" $
                eval ((Atom 5 :-: Atom 1) :-: Atom 0 :-: Atom 3)
                    `shouldBe` Atom 1

    describe "parsing, constant function" $ do

        it "*[2 1 3] -> 3" $
            parseEvalNoun "[2 1 3]" `shouldBe` parseNoun "3"

        it "*[[4 0 1 0 1] 1 [6 0 7]] -> [6 0 7]" $
            parseEvalNoun "[[4 0 1 0 1] 1 [6 0 7]]" `shouldBe` parseNoun "[6 0 7]"

    describe "substitute operator" $

        it "*[5 2 [4 0 1] 1 [4 0 1]] -> 7" $
            parseEvalNoun "[5 2 [4 0 1] 1 [4 0 1]]" `shouldBe` parseNoun "[7]"
