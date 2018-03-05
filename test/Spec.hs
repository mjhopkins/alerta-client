{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import qualified Alerta.Golden       as Golden
import qualified Alerta.Hedgehog     as Hedgehog
import           Test.Tasty          (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
  defaultMain $ testGroup "tests"
    [ testGroup "Golden tests" Golden.tests
    , testGroup "Hedgehog" Hedgehog.tests
    ]
