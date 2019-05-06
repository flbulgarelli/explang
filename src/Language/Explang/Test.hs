{-# LANGUAGE DeriveGeneric #-}

module Language.Explang.Test (
  Test (..)) where

import GHC.Generics
import Language.Explang.Expectation (Expectation)

data Test =
  Test {
    name :: String,
    expectation :: Expectation
  } deriving (Eq, Show, Generic)
