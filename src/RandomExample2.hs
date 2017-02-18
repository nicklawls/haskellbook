module RandomExample2 where

import           Control.Applicative (liftA3)
import           Control.Monad       (replicateM)
import           System.Random

data Die =
      DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

-- intToDie :: Int -> Die
-- intToDie i =
--   case i of
--     1 -> DieOne
--     2 -> DieTwo
--     3 -> DieThree
--     4 -> DieFour
--     5 -> DieFive
--     6 -> DieSix
--     _ -> error $ "int to die got non 1-6 integer"
--
-- rollDie :: State StdGen Die
-- rollDie = state $ do
--   (n, s) <- randomR (1,6)
--   return (intToDie n, s)
--
-- rollDie' :: State StdGen Die
-- rollDie' =
--   intToDie <$> state (randomR (1,6))
--
-- rollDieThrice :: State StdGen (Die, Die, Die)
-- rollDieThrice =
--   liftA3 (,,) rollDie' rollDie' rollDie'
--
-- infiniteDie :: State StdGen [Die]
-- infiniteDie = repeat <$> rollDie
--
-- nDie :: Int -> State StdGen [Die]
-- nDie n = replicateM n rollDie
--
--
-- rollsToCountLogged :: Int -> StdGen -> (Int, [Die])
-- rollsToCountLogged n g = reverse <$> (go (0, []) 0 g)
--   where go :: (Int, [Die]) -> Int -> StdGen -> (Int, [Die])
--         go (sum, logged) count gen
--           | sum >= n = (count, logged)
--           | otherwise =
--               let (die, nextGen) = randomR (1,6) gen
--               in go ((sum + die), intToDie die : logged) (count + 1) nextGen



newtype Moi s a =
  Moi {runMoi :: s -> (s,a)}

instance Functor (Moi s) where
  fmap f (Moi runMoi) = Moi (fmap f . runMoi)

instance Applicative (Moi s) where
  pure a = (Moi $ \s -> (s,a))
  (Moi f) <*> (Moi g) = Moi (\s -> let (s', h) = f s in fmap h (g s'))

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $ \s ->
    let (s', a) = f s
        (Moi h) = g a
    in
        h s'

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ const (s, ())

exec :: Moi s a -> s -> s
exec st =  fst . runMoi st

eval :: Moi s a -> s -> a
eval st = snd . runMoi st


modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> (f s, ())
