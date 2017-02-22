module Log where


import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Map (Map)
import System.IO
import Data.Time

import qualified Data.Map as M

-- I finally get what "Abstract Syntax Tree" means
data Log = Log Day [Entry]
  deriving (Show, Eq, Ord)

data Entry = Entry TimeOfDay Activity
  deriving (Show, Eq, Ord)


type Activity = String


comment :: Parser ()
comment = do
  string "--"
  skipMany (notChar '\n')


int = fromInteger <$> integer
int' = fromInteger <$> integer'


date :: Parser Day
date = do
  token (char '#')
  d <- fromGregorian <$> (integer <* char '-') <*> (int <* char '-') <*> int'
  skipMany (char ' ')
  skipOptional comment
  newline
  return d


entry :: Parser Entry
entry = do
  hour <- int
  char ':'
  minute <- int
  activity <- notChar '\n' `manyTill` (optional comment *> newline)
  return $ Entry (TimeOfDay hour minute 0) activity


aLog :: Parser Log
aLog = Log <$> date <*> many entry <* skipMany (optional comment *> newline)


-- time spent in each activity
-- average time spent per activity per day

type LogMap = Map Day (Map Activity [(Start, End)])
type Start = TimeOfDay
type End = TimeOfDay


mkLogMap :: [Log] -> LogMap
mkLogMap logs =
  M.fromList $
    fmap (\(Log date entries) -> (date, error "map activities to intervals")) logs

timeSpent :: LogMap -> [(Activity, Int)] -- Activity -> Time spent in minutes
timeSpent = undefined

avgTime :: LogMap -> [(Activity, Day, Int)] -- activit, date, time spent that day
avgTime = undefined

main :: IO ()
main = do
 Just logs <- parseFromFile (skipMany (optional comment *> newline) *> many aLog) "data.log"
 print logs
