--module Pearl02
--    ( runPearl02
--    ) where
module Pearl02 where

-- Problem 2: max surpasser count
--  a surpasser of an element of an array
--  is a greater element to the right, so x[j] is a surpasser of x[i] if i < j
--  and x[i] < x[j]. The surpasser count of an element is the number of its
--  surpassers.
runPearl02 :: IO ()
runPearl02 = putStrLn "runPearl02"

msc :: Ord a => [a] -> Int
msc = maximum . map snd . table

scount :: Int -> [Int] -> Int
scount x xs = length (filter (x <) xs)

tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = (x : xs) : tails xs

table :: Ord a => [a] -> [(a, Int)]
table [x] = [(x , 0)]
table xs = join (m - n)(table ys)(table zs)
  where m = length xs
        n = m `div` 2
        (ys, zs) = splitAt n xs

join :: Ord a => Int -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join 0 txs [] = txs
join n [] tys = tys
join n txs@((x , c) : txs') tys@((y, d) : tys')
        | x < y = (x , c + n) : join n txs tys
        | x >= y = (y, d) : join (n-1) txs tys