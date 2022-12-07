-- Day 6
import Data.List

(||>) = flip (.)

groupsN :: Int -> [a] -> [[a]]
groupsN n xs = zipWith const (take n <$> tails xs) (drop (n-1) xs)


part0 :: Int -> String -> Int
part0 n = groupsN n ||> zip [n..]  -- group characters in runs of n, zip them with # of processed chars
      ||> dropWhile (\(_, s) -> (length $ nub s) /= length s)  -- find the first run with n unique chars
      ||> head ||> fst  -- get how many chars were processed.

part1 = part0 4
part2 = part0 14


main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
