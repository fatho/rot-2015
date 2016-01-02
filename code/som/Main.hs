module Main where

import SOM
import System.Random.Mersenne.Pure64
import Data.Random
import Linear

randomColor :: (Monad m) => RVarT m (V3 Double)
randomColor = V3 <$> channel <*> channel <*> channel where
  channel = uniformT 0 1

main :: IO ()
main = do
  mt <- newPureMT
  som <- newSOM 128 128 randomColor mt
  putStrLn "Hallo"
