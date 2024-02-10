{-# OPTIONS -Wall -Werror #-}

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ (2 :: Int)
   in sideArea + 2 * topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ (2 :: Int)]
