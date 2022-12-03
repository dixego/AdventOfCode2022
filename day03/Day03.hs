-- Day 3
import Data.Set (Set, intersection)
import qualified Data.Set as Set
import Data.Char (ord)
import Data.List.Split (chunksOf)

(||>) = flip (.)

priority :: Char -> Int
priority c
  | ascii >= 65 && ascii <= 90 = ascii - 65 + 27
  | otherwise = ascii - 97 + 1
  where ascii = ord c

part1' :: [String] -> [[Char]]
part1' = map (\l -> splitAt (length l `div` 2) l) ||> 
        map (\(a,b) -> Set.toList $ (Set.fromList a) `intersection` (Set.fromList b))

part2' :: [String] -> [[Char]]
part2' = map Set.fromList 
        ||> chunksOf 3 
        ||> map (\[a,b,c] -> Set.toList $ a `intersection` b `intersection` c)

part0 :: ([String] -> [[Char]]) -> String -> Int
part0 part = lines ||> part ||> concat ||> map priority ||> sum

part1 :: String -> Int
part1 = part0 part1'
part2 :: String -> Int
part2 = part0 part2'

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
