-- Day 2 but just using numbers

(||>) = flip (.)
type Choice = Int -- Rock = 0; Paper = 1; Scissors = 2

choiceScore :: Choice -> Int
choiceScore = (+1)

matchScore :: Choice -> Choice -> Int
matchScore x y -- x = me, y == against
  | x == y = 3
  | x == ((y+1) `mod` 3) = 6
  | otherwise = 0

matchScore' :: Choice -> Int -> Int
matchScore' a 0 = choiceScore ((a-1)`mod`3)
matchScore' a 1 = 3 + choiceScore a
matchScore' a 2 = 6 + (choiceScore $ ((a+1)`mod`3))

stringToChoice :: String -> Choice
stringToChoice s
  | s `elem` ["A", "X"] = 0
  | s `elem` ["B", "Y"] = 1
  | s `elem` ["C", "Z"] = 2

part1 :: String -> Int
part1 = lines ||>
        map words ||>
        map (map stringToChoice) ||>
        map (\[against, me] -> matchScore me against + choiceScore me) ||>
        sum

part2 :: String -> Int
part2 = lines ||>
        map words ||>
        map (map stringToChoice) ||>
        map (\[against, me] -> matchScore' against me) ||>
        sum

main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
