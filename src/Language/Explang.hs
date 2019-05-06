module Language.Explang (
  parseExpectation,
  parseExpectations,
  parseExpectations',
  module Language.Explang.Expectation) where

import Codec.Binary.UTF8.String (encode)
import Language.Explang.Parser (parse)
import Language.Explang.Lexer (evalP)

import Language.Explang.Expectation

parseExpectation :: String -> Expectation
parseExpectation = head . parseExpectations

parseExpectations :: String -> [Expectation]
parseExpectations = zipWith overrideDescription [0..] . either error id . parseExpectations'
    where
      overrideDescription :: Int -> Expectation -> Expectation
      overrideDescription number e@Expectation { description = ""} = e { description = "E" ++ show number }
      overrideDescription _      e                                 = e

parseExpectations' :: String -> Either String [Expectation]
parseExpectations' = evalP parse . encode

