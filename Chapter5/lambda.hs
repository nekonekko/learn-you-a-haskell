-- import HigherOrder (chain)

-- コラッツ列
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | otherwise = n : chain (n * 3 + 1)

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

flip'' :: (a -> a -> c) -> a -> a -> c
flip'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
-- sum' = foldl (\acc x -> acc + x) 0
sum' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldr (\x acc -> x == y || acc) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

-- myfoldl :: (a -> b -> a) -> a -> [b] -> a
-- myfoldl _ acc [] = acc
-- myfoldl f acc (x : xs) = myfoldl f (f acc x) xs

-- myfoldl' :: (a -> b -> a) -> a -> [b] -> a
-- myfoldl' _ acc [] = acc
-- myfoldl' f acc (x : xs) =
--   let currentValue = f acc x
--    in myfoldl' f currentValue xs

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1
