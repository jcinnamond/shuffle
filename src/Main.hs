module Main where

import Data.List (sortBy)
import GHC.Clock (getMonotonicTime)
import System.Random (Random (randoms), StdGen, mkStdGen)

shuffle :: StdGen -> [a] -> [a]
shuffle g xs = fst <$> sortMapped snd (zip xs idxs)
 where
  idxs = randoms g :: [Int]

sortMapped :: Ord b => (a -> b) -> [a] -> [a]
sortMapped f = sortBy (\x y -> f x `compare` f y)

shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
  g <- mkStdGen . floor <$> getMonotonicTime
  pure $ shuffle g xs

main :: IO ()
main = do
  items <- words <$> getContents
  shuffled <- shuffleIO items
  putStr $ unlines shuffled
