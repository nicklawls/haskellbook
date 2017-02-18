{-# LANGUAGE OverloadedStrings #-}

module BT where

import           Control.Applicative
import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString            (ByteString)
import           Text.Parsec                (Parsec, parseTest)
import           Text.Trifecta              hiding (parseTest)


trifP :: Show a => Parser a -> String -> IO ()
trifP p i =
  print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP p i = print $ parseOnly p i


nobackParse :: (Monad m, CharParsing m) => m Char
nobackParse =
  (char '1' >> char '2') <|> char '3'


tryParse :: (Monad m, CharParsing m) => m Char
tryParse = try (char '1' >> char '2') <|> char '3'



main :: IO ()
main = do
  -- trifecta
  trifP nobackParse "12"
  trifP tryParse "1"

  -- parsec
  parsecP nobackParse "12"
  parsecP tryParse "1"

  -- attoparsec
  attoP nobackParse "12"
  attoP tryParse "3"
