module Math where

logFactorial :: (Ord a, Floating a) => a -> a
logFactorial n
  | n < 2 = 0
  | otherwise = a + b + logPiHalf where
      a = n * log n - n
      b = log (n * (1 + 4 * n * (1 + 2 * n))) / 6
      logPiHalf = log pi / 2

logPoisson :: (Ord a, Floating a) => a -> a -> a
logPoisson mean k = k * log mean - mean - logFactorial k
