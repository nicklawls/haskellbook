{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import           Control.Applicative
import           Data.Attoparsec.Text (parseOnly)
import           Data.Ratio           ((%))
import           Data.String          (IsString)
import           Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = virtuousFraction

virtuousFraction :: (Monad m, TokenParsing m) => m Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denominator can't be zero"
    _ -> return (numerator % denominator)

data FracDec =
    Frac Rational
  | Dec Integer
  deriving (Show)

fracDec :: (Monad m, TokenParsing m) => m FracDec
fracDec = Frac <$> try virtuousFraction <|> Dec <$> decimal

main :: IO ()
main = do
  print $ parseOnly parseFraction badFraction
  print $ parseOnly parseFraction shouldWork
  print $ parseOnly parseFraction shouldAlsoWork
  print $ parseOnly parseFraction alsoBad

  print $ parseString parseFraction mempty badFraction
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad








testVirtuous :: IO ()
testVirtuous = do
  print $ parseString virtuousFraction mempty shouldWork
  print $ parseString virtuousFraction mempty shouldAlsoWork
  print $ parseString virtuousFraction mempty alsoBad
  print $ parseString virtuousFraction mempty badFraction
