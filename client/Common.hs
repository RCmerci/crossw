module Common (
  raceMany,
  raceList,
  raceListTimeout,
  timeout,
  while,
  ) where

import Control.Monad
import Control.Concurrent.Async as CCA
import Control.Concurrent



raceMany :: [IO a] -> IO (Maybe a)
raceMany [] = return Nothing
raceMany (h:t) = Just <$>
  foldl (\r a ->
          (\e -> case e of
            Left  a -> a
            Right a -> a) <$> CCA.race a r)
  h t

raceList :: (a -> IO b) -> [a] -> IO (Maybe b)
raceList f l =
  raceMany (f <$> l)

raceListTimeout :: Int -> (a -> IO b) -> [a] -> IO (Maybe b)
raceListTimeout t f l =
  join <$> (timeout t $ raceList f l)


timeout :: Int -> IO a -> IO (Maybe a)
timeout n m
    | n <  0    = fmap Just m
    | n == 0    = return Nothing
    | otherwise = do
        r <- CCA.race (threadDelay n) m
        case r of
          Left _  -> return Nothing
          Right a -> return (Just a)

while :: (a -> Bool) -> (a -> IO a) -> a -> IO a
while praed funktion x
    | praed x   = do
        y <- funktion x
        while praed funktion y
    | otherwise = return x
