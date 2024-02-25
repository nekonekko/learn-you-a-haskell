{-# OPTIONS -Wall -Werror #-}

import Data.Char
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

-- encode offset = map (chr . (+ offset) . ord)

decode :: Int -> String -> String
-- decode offset = map (\c -> chr $ ord c - offset)
decode shift = encode (negate shift)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

findTo40 :: Maybe Int
findTo40 = find (\x -> digitSum x == 40) [1 ..]

findTo :: Int -> Maybe Int
findTo n = find (\x -> digitSum x == n) [1 ..]