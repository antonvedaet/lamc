module Main where

import Lib
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "alphaConvert" $ do
        it "renames abstraction binder" $
            do
                alphaConvert
                    (Abstraction 'x' (Variable 'x'))
                    'y'
                `shouldBe` Just (Abstraction 'y' (Variable 'y'))

        it "does not rename free variables" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Application
                            (Variable 'x')
                            (Variable 'z')
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Application
                            (Variable 'y')
                            (Variable 'z')
                        )
                    )

        it "rejects a new binder that would capture a free variable" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Application
                            (Variable 'x')
                            (Variable 'y')
                        )
                    )
                    'y'
                `shouldBe` Nothing

        it "does not rename variables hidden by nested binder" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Abstraction
                            'x'
                            (Variable 'x')
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Abstraction
                            'x'
                            (Variable 'x')
                        )
                    )

        it "avoids variable capture" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Abstraction
                            'y'
                            (Variable 'x')
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Abstraction
                            'a'
                            (Variable 'y')
                        )
                    )

        it "renames every bound occurrence in a nested application" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Application
                            (Variable 'x')
                            ( Application
                                (Variable 'x')
                                (Variable 'z')
                            )
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Application
                            (Variable 'y')
                            ( Application
                                (Variable 'y')
                                (Variable 'z')
                            )
                        )
                    )

        it "keeps free occurrence with the same name outside nested binder untouched" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Application
                            ( Abstraction
                                'x'
                                (Variable 'x')
                            )
                            (Variable 'x')
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Application
                            ( Abstraction
                                'x'
                                (Variable 'x')
                            )
                            (Variable 'y')
                        )
                    )

        it "avoids capture when conflicting abstraction is inside application" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Application
                            ( Abstraction
                                'y'
                                ( Application
                                    (Variable 'x')
                                    (Variable 'y')
                                )
                            )
                            (Variable 'x')
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Application
                            ( Abstraction
                                'a'
                                ( Application
                                    (Variable 'y')
                                    (Variable 'a')
                                )
                            )
                            (Variable 'y')
                        )
                    )

        it "avoids capture in both branches of an application" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Application
                            ( Abstraction
                                'y'
                                (Variable 'x')
                            )
                            ( Abstraction
                                'y'
                                (Variable 'x')
                            )
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Application
                            ( Abstraction
                                'a'
                                (Variable 'y')
                            )
                            ( Abstraction
                                'a'
                                (Variable 'y')
                            )
                        )
                    )

        it "does not rename occurrence shadowed several levels deep" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Abstraction
                            'z'
                            ( Abstraction
                                'x'
                                ( Application
                                    (Variable 'x')
                                    (Variable 'z')
                                )
                            )
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Abstraction
                            'z'
                            ( Abstraction
                                'x'
                                ( Application
                                    (Variable 'x')
                                    (Variable 'z')
                                )
                            )
                        )
                    )

        it "renames occurrence before shadowing but not after shadowing" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Application
                            (Variable 'x')
                            ( Abstraction
                                'x'
                                (Variable 'x')
                            )
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Application
                            (Variable 'y')
                            ( Abstraction
                                'x'
                                (Variable 'x')
                            )
                        )
                    )

        it "handles multiple nested binders with different names" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Abstraction
                            'a'
                            ( Abstraction
                                'b'
                                ( Application
                                    (Variable 'x')
                                    ( Application
                                        (Variable 'a')
                                        (Variable 'b')
                                    )
                                )
                            )
                        )
                    )
                    'z'
                `shouldBe` Just
                    ( Abstraction
                        'z'
                        ( Abstraction
                            'a'
                            ( Abstraction
                                'b'
                                ( Application
                                    (Variable 'z')
                                    ( Application
                                        (Variable 'a')
                                        (Variable 'b')
                                    )
                                )
                            )
                        )
                    )

        it "chooses a fresh variable when a is already used" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Abstraction
                            'y'
                            ( Application
                                (Variable 'x')
                                (Variable 'a')
                            )
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Abstraction
                            'b'
                            ( Application
                                (Variable 'y')
                                (Variable 'a')
                            )
                        )
                    )

        it "chooses a later fresh variable when several names are already used" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Abstraction
                            'y'
                            ( Application
                                (Variable 'x')
                                ( Application
                                    (Variable 'a')
                                    (Variable 'b')
                                )
                            )
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Abstraction
                            'c'
                            ( Application
                                (Variable 'y')
                                ( Application
                                    (Variable 'a')
                                    (Variable 'b')
                                )
                            )
                        )
                    )

        it "handles conflict several abstractions deep" $
            do
                alphaConvert
                    ( Abstraction
                        'x'
                        ( Abstraction
                            'z'
                            ( Abstraction
                                'y'
                                ( Application
                                    (Variable 'x')
                                    ( Application
                                        (Variable 'y')
                                        (Variable 'z')
                                    )
                                )
                            )
                        )
                    )
                    'y'
                `shouldBe` Just
                    ( Abstraction
                        'y'
                        ( Abstraction
                            'z'
                            ( Abstraction
                                'a'
                                ( Application
                                    (Variable 'y')
                                    ( Application
                                        (Variable 'a')
                                        (Variable 'z')
                                    )
                                )
                            )
                        )
                    )

        it "leaves plain variable unchanged" $
            do
                alphaConvert
                    (Variable 'x')
                    'y'
                `shouldBe` Just (Variable 'x')

        it "leaves plain application unchanged" $
            do
                alphaConvert
                    ( Application
                        (Variable 'x')
                        (Variable 'y')
                    )
                    'z'
                `shouldBe` Just
                    ( Application
                        (Variable 'x')
                        (Variable 'y')
                    )

    describe "betaReduction" $ do
        it "substitutes the argument for the bound variable" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        (Variable 'x')
                        (Variable 'y')
                    )
                `shouldBe` Variable 'y'

        it "leaves variables that are not bound by the redex unchanged" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        (Variable 'z')
                        (Variable 'y')
                    )
                `shouldBe` Variable 'z'

        it "substitutes through both sides of an application" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        ( Application
                            (Variable 'x')
                            ( Application
                                (Variable 'z')
                                (Variable 'x')
                            )
                        )
                        (Variable 'y')
                    )
                `shouldBe` Application
                    (Variable 'y')
                    ( Application
                        (Variable 'z')
                        (Variable 'y')
                    )

        it "can substitute a compound argument" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        ( Application
                            (Variable 'x')
                            (Variable 'z')
                        )
                        ( Abstraction
                            'y'
                            (Variable 'y')
                        )
                    )
                `shouldBe` Application
                    ( Abstraction
                        'y'
                        (Variable 'y')
                    )
                    (Variable 'z')

        it "does not substitute under an abstraction that rebinds the same variable" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        ( Abstraction
                            'x'
                            ( Application
                                (Variable 'x')
                                (Variable 'z')
                            )
                        )
                        (Variable 'y')
                    )
                `shouldBe` Abstraction
                    'x'
                    ( Application
                        (Variable 'x')
                        (Variable 'z')
                    )

        it "substitutes under an abstraction with a different binder" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        ( Abstraction
                            'z'
                            ( Application
                                (Variable 'x')
                                (Variable 'z')
                            )
                        )
                        (Variable 'y')
                    )
                `shouldBe` Abstraction
                    'z'
                    ( Application
                        (Variable 'y')
                        (Variable 'z')
                    )

        it "renames an inner binder to avoid capturing a free variable from the argument" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        ( Abstraction
                            'y'
                            (Variable 'x')
                        )
                        (Variable 'y')
                    )
                `shouldBe` Abstraction
                    'z'
                    (Variable 'y')

        it "chooses a later fresh binder when the next name is already used" $
            do
                betaReduction
                    ( BetaRedex
                        'x'
                        ( Abstraction
                            'y'
                            ( Application
                                (Variable 'x')
                                (Variable 'z')
                            )
                        )
                        (Variable 'y')
                    )
                `shouldBe` Abstraction
                    'a'
                    ( Application
                        (Variable 'y')
                        (Variable 'z')
                    )
