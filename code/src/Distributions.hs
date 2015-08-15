{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Distributions where

import Data.Monoid
import Data.Group
import Data.Pointed
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

import Math

class Distribution a p d where
  probability :: d a -> a -> p

data Poisson a = Poisson { poissonCount :: Int, poissonSum :: a }
                 deriving (Show, Eq, Read)

data Gaussian a = Gaussian { gaussianCount :: Int, gaussianSum :: a, gaussianVarianceN :: a }
                  deriving (Show, Eq, Read)

data Bernoulli a = Bernoulli { bernoulliPositive :: Int, bernoulliTotal :: Int }

instance Num a => Monoid (Poisson a) where
  mempty = Poisson 0 0

  mappend (Poisson m l1) (Poisson n l2)
    | total == 0 = Poisson 0 0
    | otherwise  = Poisson total (l1 + l2)
    where
      total = m+n

instance Num a => Group (Poisson a) where
  invert (Poisson n s) = Poisson (negate n) (negate s)

instance Pointed Poisson where
  point = Poisson 1

instance (Real a, Ord p, Floating p) => Distribution a p Poisson where
  probability (Poisson n s) k = exp (logPoisson (realToFrac s / realToFrac n) (realToFrac k))

batch :: (Foldable f, Pointed p, Monoid (p a)) => f a -> p a
batch = Foldable.foldMap point
