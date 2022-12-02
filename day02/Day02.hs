-- Day 2

(|>) = flip ($)
(||>) = flip (.)

data Choice = Rock | Paper | Scissors deriving Eq

choiceFromString :: String -> Choice
choiceFromString s
  | s `elem` ["A", "X"] = Rock
  | s `elem` ["B", "Y"] = Paper
  | s `elem` ["C", "Z"] = Scissors
  | otherwise = error "what?"

choiceScore :: Choice -> Int
choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3

matchScore :: Choice -> Choice -> Int
matchScore Paper Rock = 6
matchScore Rock Scissors = 6
matchScore Scissors Paper = 6
matchScore x y | x == y = 3
matchScore _ _ = 0

matchScore2 :: Choice -> String -> Int
matchScore2 x "Y" = 3 + choiceScore x
matchScore2 x "X" = choiceScore $ losesAgainst x
  where losesAgainst Rock = Scissors
        losesAgainst Scissors = Paper
        losesAgainst Paper = Rock
matchScore2 x "Z" = 6 + (choiceScore $ winsAgainst x)
  where winsAgainst Rock = Paper 
        winsAgainst Paper = Scissors 
        winsAgainst Scissors = Rock

part1 :: String -> Int
part1 = lines ||> 
        map (\l -> let [against, me] = words l in (choiceFromString against, choiceFromString me)) ||> 
        map (\(against, me) -> choiceScore me + matchScore me against) ||> 
        sum

part2 :: String -> Int
part2 = lines ||>
        map (\l -> let [against, me] = words l in (choiceFromString against, me)) ||>
        map (\(against, me) -> matchScore2 against me) ||>
        sum

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
