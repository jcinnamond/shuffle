module Main where

import Data.List (sortBy)
import GHC.Clock (getMonotonicTime)
import ShufflePluck (shufflePluck)
import System.Random (Random (randoms), RandomGen, mkStdGen)
import System.Random.Stateful (StatefulGen)

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g xs = fst <$> sortMapped snd (zip xs idxs)
 where
  idxs = randoms g :: [Int]

sortMapped :: Ord b => (a -> b) -> [a] -> [a]
sortMapped f = sortBy (\x y -> f x `compare` f y)

main :: IO ()
main = do
  items <- words <$> getContents
  g <- mkStdGen . floor <$> getMonotonicTime
  putStrLn "----- shuffle"
  putStr $ unlines $ shuffle g items
  putStrLn "----- shufflePluck"
  putStr $ unlines $ shufflePluck g items
