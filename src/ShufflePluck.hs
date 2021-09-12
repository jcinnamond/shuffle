{-# LANGUAGE ScopedTypeVariables #-}

module ShufflePluck (
    shufflePluck,
    pluck,
) where

import System.Random (Random (randomR), StdGen)

shufflePluck :: StdGen -> [a] -> [a]
shufflePluck g xs = shufflePluck' g xs []

shufflePluck' :: StdGen -> [a] -> [a] -> [a]
shufflePluck' _ [] acc = acc
shufflePluck' g xs acc =
    let (x, rest) = pluck pos xs
        (pos :: Int, g') = randomR (0, length xs - 1) g
     in shufflePluck' g' rest (x : acc)

pluck :: Int -> [a] -> (a, [a])
pluck p xs = (xs !! p, take p xs ++ drop (p + 1) xs)
