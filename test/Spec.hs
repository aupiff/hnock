{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import HNock

evalTest s1 s2 = parseEvalNoun s1 `shouldBe` parseNoun s2

main :: IO ()
main = hspec $ do

    describe "parsing" $

        it "parse [5 6 [1 0] [1 3] [1 4]]" $
            parseNoun "[5 6 [1 0] [1 3] [1 4]]" `shouldBe`
                Right (Atom 5 :-: Atom 6 :-: (Atom 1 :-: Atom 0) :-: (Atom 1 :-: Atom 3)
                                         :-: (Atom 1 :-: Atom 4))

    describe "tree lookup" $ do

        it "*[5 0 1] -> 5" $
            eval (Atom 5 :-: Atom 0 :-: Atom 1)
                `shouldBe` Right (Atom 5)

        it "*[[5 1] 0 1] -> [5 1]" $
            eval ((Atom 5 :-: Atom 1) :-: Atom 0 :-: Atom 1)
                `shouldBe` Right (Atom 5 :-: Atom 1)

        it "*[[5 1] 0 2] -> 5" $
                eval ((Atom 5 :-: Atom 1) :-: Atom 0 :-: Atom 2)
                    `shouldBe` Right (Atom 5)

        it "*[[5 1] 0 3] -> 1" $
                eval ((Atom 5 :-: Atom 1) :-: Atom 0 :-: Atom 3)
                    `shouldBe` Right (Atom 1)

    describe "constant function" $ do

        it "*[2 1 3] -> 3" $
            evalTest "[2 1 3]" "3"

        it "*[[4 0 1 0 1] 1 [6 0 7]] -> [6 0 7]" $
            evalTest "[[4 0 1 0 1] 1 [6 0 7]]" "[6 0 7]"

    describe "substitute operator" $

        it "*[5 2 [4 0 1] 1 [4 0 1]] -> 7" $
            evalTest "[5 2 [4 0 1] 1 [4 0 1]]" "[7]"

    describe "? operator" $ do

        it "*[4 3 1 [2 2]] -> 0" $
            evalTest "[4 3 1 [2 2]]" "[0]"

        it "*[4 3 1 2] -> 1" $
            evalTest "[4 3 1 2]" "[1]"

    describe "= operator" $ do

        it "*[5 5 5] -> 0" $ evalTest "[5 5 5]" "[0]"

        it "*[6 5 5] -> 0" $ evalTest "[6 5 5]" "[1]"

    describe "6 macro" $ do

        it "*[5 [6 [1 0] [1 3] [1 4]]] -> 3" $ evalTest "[5 [6 [1 0] [1 3] [1 4]]]" "[3]"

        it "*[5 [6 [1 0] [4 0 1] [4 0 1]]] -> 6" $ evalTest "[5 [6 [1 0] [4 0 1] [4 0 1]]]" "[6]"
    describe "7 macro" $

        it "composes add" $ evalTest "[42 [7 [4 0 1] [4 0 1]]]" "[44]"

    describe "8 macro" $

        it "composes add w/ 8" $ evalTest "[42 [8 [4 0 1] [0 1]]]" "[43 42]"
