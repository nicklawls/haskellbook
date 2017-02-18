module Trans where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad

rDec :: Num a => Reader a a
rDec =
  fmap (flip (-) 1) ask

rShow :: Show a => ReaderT a Identity String
rShow =
  show <$> ask

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  a <- ask
  liftIO $ print ("Hi: " ++ show a)
  return (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  state <- get
  liftIO $ print ("Hi: " ++ show state)
  put (state + 1)
  return (show state)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "Say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)