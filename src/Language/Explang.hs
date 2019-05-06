module Language.Explang (
  parseExpectation,
  parseTest,
  parseTests,
  parseTests',
  module Language.Explang.Test) where

import Codec.Binary.UTF8.String (encode)
import Language.Explang.Parser (parse)
import Language.Explang.Lexer (evalP)

import Language.Explang.Test
import Language.Explang.Expectation (Expectation)

parseExpectation :: String -> Expectation
parseExpectation = expectation . parseTest . ("test:"++)

parseTest :: String -> Test
parseTest = head . parseTests

parseTests :: String -> [Test]
parseTests = zipWith overrideName [0..] . either error id . parseTests'
    where
      overrideName :: Int -> Test -> Test
      overrideName number e@Test { name = ""} = e { name = "E" ++ show number }
      overrideName _      e                   = e

parseTests' :: String -> Either String [Test]
parseTests' = evalP parse . encode

