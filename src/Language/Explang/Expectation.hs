{-# LANGUAGE DeriveGeneric #-}

module Language.Explang.Expectation (
  Expectation (..),
  Scope (..),
  Flags (..),
  Count (..),
  Matcher (..),
  Binding (..),
  Predicate (..),
  Query (..),
  noFlags,
  intransitiveFlag) where

import GHC.Generics

data Expectation =
  Expectation {
    description :: String,
    flags :: Flags,
    scope :: Scope,
    query :: Query,
    count :: Count
  } deriving (Eq, Show, Generic)

data Query
  = Inspection { inspection :: String, binding :: Binding, matcher :: Matcher }
  | Not Query
  | Or Query Query
  | And Query Query
  deriving (Eq, Show, Generic)


data Scope
  = Unscoped
  | Scoped { context :: String }
  deriving (Eq, Show, Generic)

data Flags = Flags { intransitive :: Bool } deriving (Eq, Show, Generic)

data Binding
  = Any
  | Named { identifier :: String }
  | Like { identifier :: String }
  | Except { identifier :: String }
  | AnyOf { identifiers :: [String] }
  deriving (Eq, Show, Generic)

data Matcher
  = Unmatching
  | Matching { expectations :: [Predicate] }
  deriving (Eq, Show, Generic)

data Count
  = AnyCount
  | AtLeast { times :: Int }
  | AtMost { times :: Int }
  | Exactly { times :: Int }
  deriving (Eq, Show, Generic)


data Predicate
  = That { expectation :: Expectation }
  | IsNumber Double
  | IsString String
  | IsSymbol String
  | IsChar Char
  | IsTrue
  | IsFalse
  | IsNil
  | IsSelf
  | IsMath
  | IsLogic
  deriving (Eq, Show, Generic)

noFlags :: Flags
noFlags = Flags False

intransitiveFlag :: Flags
intransitiveFlag = Flags True

