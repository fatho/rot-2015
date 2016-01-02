{-# LANGUAGE FlexibleContexts #-}
module SOM where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.ST
import Data.STRef
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VU
import Linear
import qualified Data.Random as Rand

data SOM s a = SOM { somBounds :: V2 Int, somData :: VU.MVector s a }

initializeSOM :: (PrimMonad m) => SOM (PrimState m) a -> (Int -> Int -> m a) -> m ()
initializeSOM SOM { somBounds = V2 w h, somData = vec} gen =
  forM_ [0..h-1] $ \y ->
    forM_ [0..w-1] $ \x -> do
      val <- gen x y
      VU.unsafeWrite vec (y * w + x) val

newSOM_ :: PrimMonad m => Int -> Int -> m (SOM (PrimState m) a)
newSOM_ w h = do
  arr <- VU.new (w * h)
  return SOM { somBounds = V2 w h, somData = arr }

sampleST :: (Rand.MonadRandom (State r)) => STRef s r -> Rand.RVar a -> ST s a
sampleST ref var = do
  (t,s) <- Rand.sampleState var <$> readSTRef ref
  writeSTRef ref s
  return t

withRandST :: (Rand.MonadRandom (State r)) => Rand.RVar a -> r -> (ST s a -> ST s b) -> ST s b
withRandST rvar state0 cont = do
  ref <- newSTRef state0
  cont (sampleST ref rvar)

newSOM :: (PrimMonad m, Rand.MonadRandom (State r)) => Int -> Int -> Rand.RVar a -> r -> m (SOM (PrimState m) a)
newSOM w h rvar rndState0 = do
  som <- newSOM_ w h
  liftPrim $ withRandST rvar rndState0 $ \gen ->
    initializeSOM som $ \_ _ -> gen
  return som

-- learn :: (PrimMonad m) => SOM (PrimState s) a -> a ->
