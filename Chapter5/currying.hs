{-# OPTIONS -Wall -Werror #-}

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

dividedByTen :: (Floating a) => a -> a
dividedByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])