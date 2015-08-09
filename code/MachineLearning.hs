{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module MachineLearning where

import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

data Gaussian a = Gaussian { gaussianMean :: a, gaussianVariance :: a, gaussianCount :: Int }

sum' :: (Num a, Foldable f) => f a -> a
sum' = Foldable.foldl' (+) 0

-- | Requires that 'sum xs == 1'
entropy :: (Eq a, Floating a, Functor f, Foldable f) => f a -> a
entropy = sum' . fmap g where
  g x
    | x == 0 = 0
    | otherwise = - x * logBase 2 x


data Attribute row = forall att . (Eq att, Enum att, Bounded att) => Attribute (row -> att)

data DecisionTree row
  = LeafNode Bool
  | forall att . (Eq att, Enum att, Bounded att)
    => DecisionNode (row -> att) [(att, DecisionTree row)]

data TrainingSet row = TrainingSet
                       { trainingEval :: row -> Bool
                       , trainingData :: [row]
                       , trainingPositive :: Int
                       , trainingNegative :: Int
                       }

makeTrainingSet :: (row -> Bool) -> [row] -> TrainingSet row
makeTrainingSet eval rows = TrainingSet eval rows pos neg where
  (pos, neg) = Foldable.foldl' (\(p, n) x -> if eval x then (p+1, n) else (p, n+1)) (0,0) rows

learnDecisionTree :: [Attribute row] -> TrainingSet row -> DecisionTree row
learnDecisionTree attrs training = undefined where
  p = trainingPositive training
  n = trainingNegative training
  totalEntropy = entropy [realToFrac p / realToFrac (p + n), realToFrac n / realToFrac (p + n)]

data Foo = Foo { raining, alternative, stay :: Bool } deriving (Show, Read, Eq, Ord)

fooData :: [Foo]
fooData =
  [ Foo True True True
  , Foo True False True
  , Foo False True False
  , Foo False False False
  ]

