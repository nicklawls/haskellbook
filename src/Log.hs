module Log () where


import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Map (Map)
import System.IO

-- time spent in each activity
-- average time spent per activity per day


data Log = Log Date [Entry]
  deriving (Show, Eq, Ord)

data Entry = Entry Hour Min Activity
  deriving (Show, Eq, Ord)

type Hour = Integer
type Min = Integer
type Activity = String

data Date = Date Year Month Day
      deriving (Show, Eq, Ord)

type Year = Integer
type Month = Integer
type Day = Integer

comment :: Parser ()
comment = do
  string "--"
  skipMany (notFollowedBy newline)

aLog :: Parser Log
aLog = do
  token (char '#')
  date <- Date <$> (integer <* char '-') <*> (integer <* char '-') <*> integer

  entries <- many $ do
    hour <- integer <* char ':'
    min <- integer
    activity <- (alphaNum <|> oneOf "!@#$%^&*-=()_+:;" <|> space) `manyTill` newline

    return $ Entry hour min activity

  return $ Log date entries


main :: IO ()
main =
  readFile "data.log" >>= return . parseString (many aLog) mempty >>= print
