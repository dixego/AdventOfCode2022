-- Day 4
import Data.List.Split (splitOn)
(||>) = flip (.)

type Range = (Int, Int)

rangesOverlap :: Range -> Range -> Bool
rangesOverlap (a1, b1) (a2, b2) = a1 <= b2 && b1 >= a2

rangesContained :: Range -> Range -> Bool
rangesContained (a1, b1) (a2, b2) = (a1 >= a2 && b1 <= b2) || (a2 >= a1 && b2 <= b1)

part0 :: String -> [(Range, Range)]
part0 = lines ||> map (\l -> let [a,b] = splitOn "," l
                                 ([a1,b1],[a2,b2]) = (splitOn "-" a, splitOn "-" b)
                              in ((read a1,read b1),(read a2,read b2)))

part0' :: ([(Range, Range)] -> [Bool]) -> String -> Int
part0' f = part0 ||> f ||> filter id ||> length

part1 :: String -> Int
part1 = part0' $ map $ uncurry rangesContained

part2 :: String -> Int
part2 = part0' $ map $ uncurry rangesOverlap

main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
