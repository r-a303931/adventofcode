module Aoc2023.Day2 (results1, part1, results2, part2) where

import Aoclibs.Lib
import Aoclibs.Parser
import Control.Applicative
import Data.Foldable
import Data.List
import Data.Monoid

data Color = Red | Green | Blue deriving (Show, Eq)

type Result = Int
type Game = (Int, [[(Int, Color)]])
type Repr = [Game]

testData :: String
testData = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
           \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
           \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
           \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
           \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n\
           \"

colorP :: Parser Color
colorP = colorFromString <$> (stringP "red" <|> stringP "green" <|> stringP "blue") where
        colorFromString "red" = Red
        colorFromString "green" = Green
        colorFromString "blue" = Blue
        colorFromString _ = undefined

gameParser :: Parser Game
gameParser = return (,) <*> (stringP "Game " *> intP <* stringP ": ") <*> gameDataParser where
  gameDataParser = sepBy (stringP "; ") subsetParser
  subsetParser = sepBy (stringP ", ") pairParser
  pairParser = return (,) <*> (intP <* charP ' ') <*> colorP

isGameValid :: (Int, Int, Int) -> Game -> Bool
isGameValid (red, green, blue) = getAll . fold . map isSubsetValid . snd where
  isSubsetValid = fold . map (All . isThrowValid)
  isThrowValid (throw, Red) = throw <= red
  isThrowValid (throw, Green) = throw <= green
  isThrowValid (throw, Blue) = throw <= blue

solve1 :: Repr -> Result
solve1 = sum . map fst . filter (isGameValid (12, 13, 14))

results1 :: Maybe [TestResult Result]
results1 = makeTestP (linesP gameParser) solve1 [(testData, 8)]

part1 :: Solution Repr Result Int
part1 = Solution { filePathP = "inputs/day2.txt"
                 , contentParser = linesP gameParser
                 , solveProblemP = solve1
                 , displaySolutionP = id
                 }

gamePower :: Game -> Int
gamePower (_, cubes) = computePower Red * computePower Green * computePower Blue where
  computePower color = foldr max 0 . concatMap (subsetPower color) $ cubes
  subsetPower color = map fst . filter ((==color) . snd)

solve2 :: Repr -> Result
solve2 = sum . map gamePower

results2 :: Maybe [TestResult Result]
results2 = makeTestP (linesP gameParser) solve2 [(testData, 2286)]

part2 :: Solution Repr Result Int
part2 = part1 { solveProblemP = solve2 }
