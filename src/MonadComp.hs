module MonadComp where

import Control.Monad (join, (>=>))


mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join $ fmap f (g a)


mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = g a >>= f


sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine


readM :: Read a => String -> IO a
readM = return . read


getAge :: Read a => String -> IO a
getAge = sayHi >=> readM


askForAge :: IO Int
askForAge = getAge "Hello, how old are you?"