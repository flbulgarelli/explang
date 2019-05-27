module Language.Explang (
  parseExpectation,
  parseTest,
  parseTests,
  parseTests',
  module Language.Explang.Test) where

import           Codec.Binary.UTF8.String (encode)
import           Language.Explang.Lexer (evalP)
import qualified Language.Explang.Parser as P

import           Language.Explang.Test
import           Language.Explang.Expectation (Expectation)

parseExpectation :: String -> Expectation
parseExpectation = wrap P.parseExpectation

parseTests :: String -> [Test]
parseTests = zipWith overrideName [0..] . wrap P.parseTests
  where
    overrideName :: Int -> Test -> Test
    overrideName number e@Test { name = ""} = e { name = "E" ++ show number }
    overrideName _      e                   = e

parseTests' :: String -> Either String [Test]
parseTests' = evalP P.parseTests . encode

parseTest :: String -> Test
parseTest = head . parseTests


wrap f = either error id . evalP f . encode
