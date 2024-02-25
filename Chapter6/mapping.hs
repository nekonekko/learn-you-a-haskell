{-# OPTIONS -Wall -Werror #-}

import Data.Char (digitToInt, isDigit)
import Data.Map qualified as Map

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter (\(k, _) -> key == k)

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

phoneBook :: Map.Map String String
phoneBook =
  Map.fromList $
    [ ("betty", "555-2938")
    , ("bonnie", "452-2928")
    , ("patsy", "493-2928")
    , ("lucille", "205-2928")
    , ("wendy", "939-8282")
    , ("penny", "853-2492")
    ]

string2didits :: String -> [Int]
string2didits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs