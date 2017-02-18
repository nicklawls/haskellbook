module Semver
    (
    ) where

import Text.Trifecta
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Foldable
import Data.Monoid

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  compare (NOSS s) (NOSI i) = GT
  compare (NOSI i) (NOSS s) = LT
  compare (NOSS s) (NOSS s2) = compare s s2
  compare (NOSI i) (NOSI i2) = compare i i2

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  Semver Major Minor Patch Release Metadata
    deriving (Show, Eq)


{- Precedence for two pre-release versions with the same major, minor, and patch
   version MUST be determined by comparing each dot separated identifier from
   left to right until a difference is found as follows:

   identifiers consisting of only digits are compared numerically and
   identifiers with letters or hyphens are compared lexically in ASCII sort order.

   Numeric identifiers always have lower precedence than non-numeric identifiers.

   A larger set of pre-release fields has a higher precedence than a smaller set, if all of the
   preceding identifiers are equal.
-}

instance Ord SemVer where
  compare (Semver major minor patch release _) (Semver major2 minor2 patch2 release2 _) =
    fold
      [ compare major major2
      , compare minor minor2
      , compare patch patch2
      , fold (zipWith compare release release2)
      , compare (length release) (length release2)
      ]


positive = mfilter (>= 0)

semVer :: Parser SemVer
semVer =
  Semver
    <$> positive integer
    <*> (dot *> positive integer)
    <*> (dot *> positive integer)
    <*> (try (hyphen *> dotSeparatedIdentifiers) <|> pure [])
    <*> (try (plus *> dotSeparatedIdentifiers) <|> pure [])


plus :: Parser Char
plus = char '+'

hyphen :: Parser Char
hyphen = char '-'

nosi :: Parser NumberOrString
nosi = NOSI <$> integer

noss :: Parser NumberOrString
noss = NOSS <$> (some (letter <|> hyphen))

dotSeparatedIdentifiers :: Parser [NumberOrString]
dotSeparatedIdentifiers =
  liftA2 (:) (noss <|> nosi) (many (try (dot *> noss) <|> dot *> nosi))


-- some -> 1 or more
-- many -> 0 or more


parseDigit :: Parser Char
parseDigit =
  satisfy (`elem` "0123456789")


charToNum :: Char -> Parser Integer
charToNum '0' = return 0
charToNum '1' = return 1
charToNum '2' = return 2
charToNum '3' = return 3
charToNum '4' = return 4
charToNum '5' = return 5
charToNum '6' = return 6
charToNum '7' = return 7
charToNum '8' = return 8
charToNum '9' = return 9
charToNum _ = fail "invalid digit"

base10Integer :: Parser Integer
base10Integer =
  foldl' (\total digit -> total * 10 + digit) 0 <$> some (parseDigit >>= charToNum)


-- Unsure how much of the parse <|> consumes
base10Integer' :: Parser Integer
base10Integer' =
  hyphen *> (negate <$> base10Integer) <|> base10Integer



type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)


parsePhone :: Parser PhoneNumber
parsePhone =
  try kind1 <|> try kind2 <|> kind3

kind1 =
  PhoneNumber <$> ((country *> int <|> int) <* hyphen) <*> (int <* hyphen) <*> int

country = string "1-"


int :: Parser Int
int = fmap fromIntegral integer

kind2 = do
  [a,b,c,d,e,f,g,h,i,j] <- some (fmap fromIntegral $ charToNum =<< digit)
  return $ PhoneNumber (a * 100 + b * 10 + c) (d * 100 + e * 10 + f) (g * 1000 + h * 100 + i * 10 + j)

kind3 = PhoneNumber <$> (char '(' *> int <* char ')') <*> (space *> int) <*> int

foo =
  " dfdfdf dfkjdfkfd\nkjdfkjdfkjdf\nkjdfkjdf\n"
