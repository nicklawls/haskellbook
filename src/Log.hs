module Log where


import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Map (Map)
import System.IO

import qualified Data.Map as M


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
  skipMany (notChar '\n')


date :: Parser Date
date = do
  token (char '#')
  d <- Date <$> (integer <* char '-') <*> (integer <* char '-') <*> integer'
  skipMany (char ' ')
  skipOptional comment
  newline
  return d

entry :: Parser Entry
entry =
  Entry
  <$> (integer <* char ':')
  <*> integer <*> (notChar '\n' `manyTill` (optional comment *> newline))


aLog :: Parser Log
aLog = Log <$> date <*> many entry <* skipMany (optional comment *> newline)


-- time spent in each activity
-- average time spent per activity per day

type LogMap = Map Date (Map Activity [(Start, End)])
type Start = (Hour, Min)
type End = (Hour, Min)


mkLogMap :: [Log] -> LogMap
mkLogMap logs =
  M.fromList $
    fmap (\(Log date entries) -> (date, error "map activities to intervals")) logs

timeSpent :: LogMap -> [(Activity, Int)] -- Activity -> Time spent in minutes
timeSpent = undefined

avgTime :: LogMap -> [(Activity, Date, Int)] -- activit, date, time spent that day
avgTime = undefined

main :: IO ()
main = do
 Just logs <-parseFromFile (skipMany (optional comment *> newline) *> many aLog) "data.log"
 print logs
