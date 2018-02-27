{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import qualified Alerta.Golden       as G
import qualified Alerta.Hedgehog     as H
-- import           Data.Monoid
import           Test.Tasty          (TestTree, defaultMain, testGroup)
-- import           Test.Tasty.Golden
import           Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
  golden <- G.tests
  -- golden <- G.goldenTests
  defaultMain $ testGroup "tests" [golden, hedgehog]

-- golden :: TestTree
-- golden = testGroup "Golden" G.tests

hedgehog :: TestTree
hedgehog = testGroup "Hedgehog" $ [] -- fmap (uncurry testProperty) H.tests
