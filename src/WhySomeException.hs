{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module WhySomeException where

import Control.Exception (ArithException(..), AsyncException(..))

import Data.Typeable


data MyException where
    MyException :: (Show e, Typeable e) => e -> MyException

instance Show MyException where
    showsPrec p (MyException e) =
        showsPrec p e


multiError  :: Int -> Either MyException Int
multiError n =
    case n of
        0 ->
            Left (MyException DivideByZero)
        1 ->
            Left (MyException StackOverflow)
        _ ->
            Right n