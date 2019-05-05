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
parseExpectations = either error id . parseExpectations'

parseExpectations' :: String -> Either String [Expectation]
parseExpectations' = evalP parse . encode

