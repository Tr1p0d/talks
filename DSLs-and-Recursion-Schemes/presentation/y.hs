y f = f $ y f

sum' f n
  | n <= 1 = 1
  | otherwise = n + f (n - 1)

fact f n
  | n <= 1 = 1
  | otherwise = n * f (n - 1)
