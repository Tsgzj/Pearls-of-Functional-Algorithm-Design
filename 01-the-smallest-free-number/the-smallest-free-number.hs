:set prompt-cont ""
import           Data.List

minfree :: [Int] -> Int
minfree xs = minform 0 (length xs, xs)

minform :: Int -> (Int, [Int]) -> Int
minform a (n, xs) | n == 0     = a
                  | m == b - a = minform b (n - m, vs)
                  | otherwise  = minform a (m, us)
 where
  (us, vs) = partition (< b) xs
  b        = a + 1 + n `div` 2
  m        = length us

main :: IO ()
main = print $ minfree [1, 2, 3, 5, 6, 7, 4]
