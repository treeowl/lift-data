{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      LiftGenericsSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

@hspec@ tests for `lift-data`.
-}
module LiftDataSpec (main, spec) where

import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat
import Test.Hspec
import Types
import Control.Exception (Exception, throw, evaluate)
import Data.Typeable (Typeable)

main :: IO ()
main = hspec spec

description :: String
description = "should equal its lifted counterpart"

data Exc = Exc deriving (Show, Typeable, Eq)
instance Exception Exc

spec :: Spec
spec = parallel $ do
    describe "genericLift" $ do
        describe "Unit" $ do
            it description $ do
                Unit `shouldBe` $(lift Unit)
                ConE 'Unit `shouldBe` runPureQ (liftQuote Unit)
            it "should throw an exception on undefined" $
                evaluate (runPureQ $ liftQuote (throw Exc :: Unit)) `shouldThrow` (== Exc)
        describe "Product" $
            it description $
                p `shouldBe` $(lift p)
        describe "Sum" $
            it description $
                s `shouldBe` $(lift s)
#if MIN_VERSION_template_haskell(2,16,0)
    describe "genericLiftTyped" $ do
        describe "Unit" $
            it description $ do
                Unit `shouldBe` $$(liftTyped Unit)
                ConE 'Unit `shouldBe` runPureQ (unTypeCode (liftTypedQuote Unit))
        describe "Product" $
            it description $
                p `shouldBe` $$(liftTyped p)
        describe "Sum" $
            it description $
                s `shouldBe` $$(liftTyped s)
#endif
