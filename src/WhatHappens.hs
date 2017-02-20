module WhatHappens where

import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar


main :: IO ()
main = do
    mv <- myData
    putMVar mv 0
    mv' <- myData -- mv' is a reference to a new mvar
    zero <- takeMVar mv' -- take from a empty mvar = deadlock
    print zero