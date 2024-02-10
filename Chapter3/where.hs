{-# OPTIONS -Wall -Werror #-}

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal."
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ (2 :: Int)
    -- skinny = 18.5
    -- normal = 25.0
    -- fat = 30.0
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials "" lastname = "Please input firstname, " ++ lastname
initials firstname "" = "Please input lastname, " ++ firstname
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ (2 :: Int)
