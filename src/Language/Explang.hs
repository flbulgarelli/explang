module Language.Explang (
  parseQuery,
  parseExpectation,
  parseExpectations,
  parseExpectations') where

import           Codec.Binary.UTF8.String (encode)
import           Language.Explang.Lexer (evalP)
import qualified Language.Explang.Parser as P

import           Language.Explang.Expectation (Query, Expectation(..))

parseQuery :: String -> Query
parseQuery = wrap P.parseQuery

parseExpectations :: String -> [Expectation]
parseExpectations = zipWith overrideName [0..] . wrap P.parseExpectations
  where
    overrideName :: Int -> Expectation -> Expectation
    overrideName number e@Expectation { name = ""} = e { name = "E" ++ show number }
    overrideName _      e                   = e

parseExpectations' :: String -> Either String [Expectation]
parseExpectations' = evalP P.parseExpectations . encode

parseExpectation :: String -> Expectation
parseExpectation = head . parseExpectations


wrap f = either error id . evalP f . encode
