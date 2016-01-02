import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import qualified Data.List as List
import Data.RVar
import Data.Random
import Linear
import Text.Printf

rotate2D :: Floating a => a -> V2 a -> V2 a
rotate2D r = (rot !*) where
  rot = V2 (V2 (cos r) (- sin r))
           (V2 (sin r) (cos r))

cluster :: V2 Double -> V2 Double -> Double -> RVar (V2 Double)
cluster center (V2 vx vy) angle = do
  x <- normal 0 vx
  y <- normal 0 vy
  return $ rotate2D angle (V2 x y) ^+^ center

sampleMany :: MonadRandom m => Int -> RVar a -> m [a]
sampleMany count = replicateM count . sampleRVar

clusterSample :: IO ()
clusterSample = do
  printf "%-5s %-5s %s\n" "x" "y" "class"
  c1 <- sampleMany 8 (cluster 1 0.5 0)
  c2 <- sampleMany 8 (cluster (V2 3 4) (V2 1 1.1) 0)
  c3 <- sampleMany 12 (cluster (V2 5 1) 1 0)
  forM_ c1 (printCluster "a")
  forM_ c2 (printCluster "b")
  forM_ c3 (printCluster "c")

printCluster :: String -> V2 Double -> IO ()
printCluster c (V2 x y) = printf "%5.2f %5.2f %s\n" x y c

pcaSample :: IO ()
pcaSample = do
  printf "%-5s %-5s\n" "x" "y"
  xs <- sampleMany 10 (normal 0 1)
  es <- sampleMany 10 (normal 0 0.1)
  let ys = zipWith (\x e -> x + e :: Double) xs es
      c = zipWith V2 xs ys
  -- c <- sampleMany 10 (cluster (V2 0 0) (V2 0.2 1) (- pi / 4))
  let normC = normalizePCA c
  forM_ normC $ \(V2 x y) -> printf "%.2f %.2f\n" x y

normalizeRange :: (Fractional a, Ord a) => [V2 a] -> [V2 a]
normalizeRange (x:xs) = map scale (x:xs) where
  (V2 (V2 xmin ymin) (V2 xmax ymax)) = List.foldl' (\(V2 tmin tmax) v -> V2 (vmin tmin v) (vmax tmax v)) (V2 x x) xs
  vmin = mzipWith min
  vmax = mzipWith max
  scale (V2 x y) = V2 ((x - xmin) / (xmax - xmin)) ((y - xmin) / (ymax - ymin))

normalizePCA :: (Floating a) => [V2 a] -> [V2 a]
normalizePCA xs = xs'' where
  mean = sum xs / realToFrac (length xs)
  xs'  = map (subtract mean) xs
  var  = sum (map (^2) xs') / realToFrac (length xs)
  xs'' = map (/ sqrt var) xs'

circleClass :: IO ()
circleClass = do
  c1 <- filter (\v -> quadrance v <= 0.9) <$> sampleMany 100 (cluster 0 1 0)
  c2 <- filter (\v -> quadrance v >= 1.1 && quadrance v <= 4) <$> sampleMany 100 (cluster 0 1 0)
  printf "%-5s %-5s %s\n" "x" "y" "class"
  forM_ c1 (printCluster "a")
  forM_ c2 (printCluster "b")

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

main :: IO ()
main = circleClass
