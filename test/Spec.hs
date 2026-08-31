module Main where

import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "alphaConvert" $ do

        it "renames abstraction binder" $ do
            alphaConvert
                (Abstraction 'x' (Variable 'x'))
                'y'
            `shouldBe`
                Abstraction 'y' (Variable 'y')

        it "does not rename free variables" $ do
            alphaConvert
                (Abstraction 'x'
                    (Application
                        (Variable 'x')
                        (Variable 'z')))
                'y'
            `shouldBe`
                Abstraction 'y'
                    (Application
                        (Variable 'y')
                        (Variable 'z'))

        it "does not rename variables hidden by nested binder" $ do
            alphaConvert
                (Abstraction 'x'
                    (Abstraction 'x'
                        (Variable 'x')))
                'y'
            `shouldBe`
                Abstraction 'y'
                    (Abstraction 'x'
                        (Variable 'x'))

        it "avoids variable capture" $ do
            alphaConvert
                (Abstraction 'x'
                    (Abstraction 'y'
                        (Variable 'x')))
                'y'
            `shouldBe`
                Abstraction 'y'
                    (Abstraction 'a'
                        (Variable 'y'))