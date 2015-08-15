{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module NaiveBayes where

import Data.Group
import Data.Monoid
import Data.Pointed
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

import Distributions

data NaiveBayes c d a = NaiveBayes { bayesData :: Map c (Int, d a), bayesCount :: Int }

classify :: (Distribution a Double d) => NaiveBayes c d a -> a -> c
classify NaiveBayes{..} features = maximumWith h (Map.toList bayesData) where
  h (cl, (num,distr)) = Maximum (probability distr features * realToFrac num / realToFrac bayesCount :: Double) cl

learn :: (Pointed d) => a -> c -> NaiveBayes c d a
learn val cl = NaiveBayes (Map.singleton cl (1, point val)) 1

data Maximum k v = NoMaximum | Maximum { maximumLabel :: k, maximumValue :: v }

instance Ord k => Monoid (Maximum k v) where
  mempty = NoMaximum
  mappend NoMaximum m = m
  mappend m NoMaximum = m
  mappend m1@(Maximum k1 _) m2@(Maximum k2 _)
    | k1 >= k2 = m1
    | otherwise = m2

maximumWith :: (Ord b, Foldable f) => (a -> Maximum b c) -> f a -> c
maximumWith f = maximumValue . Foldable.foldMap f

instance (Monoid (d a), Ord c) => Monoid (NaiveBayes c d a) where
  mempty = NaiveBayes Map.empty 0
  mappend (NaiveBayes d1 c1) (NaiveBayes d2 c2) = NaiveBayes newD (c1 + c2) where
    newD = Map.unionWith merge d1 d2
    merge (num1, p1) (num2, p2) = (num1 + num2, p1 <> p2 )

mpow :: (Monoid m, Integral a) => m -> a -> m
mpow m x
  | x < 0 = error "negative powers require a group structure"
  | x == 0 = mempty
  | x `mod` 2 == 0 = m' <> m'
  | otherwise = m' <> m' <> m
  where
    m' = mpow m (x `div` 2)

test :: NaiveBayes String Poisson Int
test = Foldable.fold
       [ learn 1 "A"
       , learn 2 "A"
       , learn 3 "A"
       , learn 10 "B"
       , learn 11 "B"
       , learn 12 "B"
       ]
