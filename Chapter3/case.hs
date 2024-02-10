{-# OPTIONS -Wall -Werror #-}

head'' :: [a] -> a
head'' xs =
  case xs of
    []      -> error "No head for empty lists!"
    (x : _) -> x

describeList :: [a] -> String
describeList ls =
  "The list is "
    ++ case ls of
      []  -> "empty."
      [_] -> "a singleton list."
      _   -> "a longer list."
