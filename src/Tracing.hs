module Tracing where


import Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes =
  inc (trace "I got eval'd" (1 + 1))
      + twice (trace "I got eval'd" (1 + 1))

howManTimes' =
  let onePlusOne = trace "I got eval'd" (1 + 1)
  in inc onePlusOne + twice onePlusOne

strictPattern :: (a,b) -> String
strictPattern (a,b) = const "Cousin It" a

lazyPattern :: (a,b) -> String
lazyPattern ~(a,b) = const "Cousin It" a