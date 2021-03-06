{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Ini where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString     (ByteString)
import           Data.List           (isSuffixOf)
import           Data.Char           (isAlpha)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta
import           System.IO
import           System.Directory

headerEx :: ByteString
headerEx = "[blah]"

newtype Header =
  Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value


parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)


skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blah\n; woot\n  \n;hah"

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)


sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section] host=wikipedia.org alias=claw
[whatisit] red=intoothandclaw
|]


data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')


parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $
    Section h (M.fromList assignments)


rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m


parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  return $ Config (foldr rollup M.empty sections)


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing


main :: IO ()
main = do
    files <- getDirectoryContents "configs"
    ascList <- forM (filter (".ini" `isSuffixOf`) files) $ \filePath -> do
        contents <- readFile ("configs/" ++ filePath)
        return (filePath, parseString parseIni mempty contents)
    print (M.fromList ascList)
    return ()





