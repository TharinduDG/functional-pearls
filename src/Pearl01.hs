--module Pearl01
--    ( runPearl01
--    ) where
module Pearl01 where

import Data.Array
import Data.List hiding ((\\))
import Data.Array.ST

-- Problem 1: smallest natural number not in a given finite set X of natural numbers.
--  The key fact for both the array-based and divide and conquer solutions
--  is that not every number in the range [0 .. length xs] can be in xs. Thus the
--  smallest number not in xs is the smallest number not in filter (≤ n)xs, where
--  n = length xs.

runPearl01 :: IO ()
runPearl01 = putStrLn "runPearl01"

minfree1 :: [Int] -> Int
minfree1 xs = head ([0..] \\ xs)

minfree2 :: [Int] -> Int
minfree2 = search . checklist

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`elem` vs) us

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
               where n = length xs

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
               where n = length xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x , k) <- assocs $ countlist xs]

checklistST :: [Int] -> Array Int Bool
checklistST xs =
  runSTArray $ do
    a <- newArray (0, n) False
    _ <- sequence_ [writeArray a x True | x <- xs, x <= n]
    return a
  where
    n = length xs

minfree :: [Int] -> Int
minfree xs = minfrom 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs) | n == 0     = a
                  | n == b - a = minfrom b (n - m, vs)
                  | otherwise  = minfrom a (m, us)
                    where (us, vs) = partition (< b) xs
                          b = a + 1 + n `div` 2
                          m = length us
