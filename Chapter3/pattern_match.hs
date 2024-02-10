{-# OPTIONS -Wall -Werror #-}

-- パターンマッチ
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = show x ++ " is not between 1 and 5!"

-- Int ではなく0以上の整数に対して定義する必要あり
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- ベクトルの足し算
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- トリプルの要素を取り出す
first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

-- リストのパターンマッチ
head' :: [a] -> a
head' [] = error "Can't tell head on an empty list, dymmy!"
head' (x : _) = x

-- as パターン
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter s@(x : _) = "The first letter of " ++ s ++ " is " ++ [x]
