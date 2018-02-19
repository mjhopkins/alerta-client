{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Golden
import Data.Monoid
import qualified Alerta.Golden as H
import qualified Alerta.Hedgehog as H

main :: IO ()
main = defaultMain $ testGroup "tests" [golden, hedgehog]

golden :: TestTree
golden = testGroup "Golden" []

hedgehog :: TestTree
hedgehog = testGroup "Hedgehog" $ fmap (uncurry testProperty) H.tests
