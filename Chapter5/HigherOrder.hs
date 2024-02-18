{-# OPTIONS -Wall -Werror #-}

-- module HigherOrder (applyTwice, zipWith', flip', map', filter', quickSort', largestDivisible, chain, numLongChains) where

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g where g x y = f y x
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x : xs) =
  let smallerOrSame = filter (<= x) xs
      larger = filter (> x) xs
   in quickSort' smallerOrSame ++ [x] ++ quickSort' larger

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..]) where p x = x `mod` 3829 == 0

-- コラッツ列
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | otherwise = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100])) where isLong xs = length xs > 15