-- AoC2022 Day 1
import Data.List

(|>) = flip ($)
(||>) = flip (.)

-- Group elements of a list splitting them by a predicate.
-- Shoutouts to StackOverflow, as ever.
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p l = map reverse $ splitWhen' l []
  where
    splitWhen' [] as = [as]
    splitWhen' (x:xs) as 
      | p x = as : splitWhen' xs []
      | otherwise = splitWhen' xs (x:as)  


max' :: Ord a => [a] -> a
max' = foldr1 (\a b -> if a > b then a else b)

-- Process input into sums of calories.
part0 :: String -> [Int]
part0 = lines ||> splitWhen (== "") ||> map (map read) ||> map sum 

-- Get the max amount of calories
part1 :: String -> Int
part1 = part0 ||> max'

-- Get the sum of the 3 highest amounts of calories.
part2 :: String -> Int
part2 = part0 ||> sort ||> reverse ||> take 3 ||> sum


main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
