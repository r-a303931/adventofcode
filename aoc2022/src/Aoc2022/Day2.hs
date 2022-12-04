module Aoc2022.Day2 (results1, part1, results2, part2) where

import Control.Applicative
import Aoclibs.Lib
import Aoclibs.Parser

data Move = Rock | Paper | Scissors deriving (Show, Eq)
data Strategy = Win | Lose | Draw deriving Show
type Repr1 = [(Move, Move)]
type Repr2 = [(Move, Strategy)]
type Result = Int

testData :: String
testData = "A Y\n\
           \B X\n\
           \C Z\n"

winningMove :: Move -> Move
winningMove Rock = Paper
winningMove Paper = Scissors
winningMove Scissors = Rock

losingMove :: Move -> Move
losingMove Rock = Scissors
losingMove Paper = Rock
losingMove Scissors = Paper

beats :: Move -> Move -> Bool
beats m1 m2 = winningMove m2 == m1

points :: Move -> Int
points Rock = 1
points Paper = 2
points Scissors = 3

roundPoints :: (Move, Move) -> Int
roundPoints (them, me) | beats me them = 6 + points me
roundPoints (them, me) | beats them me = 0 + points me
roundPoints (_,    me)                 = 3 + points me

moveParser :: Parser Move
moveParser = charMapP [('A', Rock), ('B', Paper), ('C', Scissors)] <|> charMapP [('X', Rock), ('Y', Paper), ('Z', Scissors)]

fileParser1 :: Parser Repr1
fileParser1 = sepBy (charP '\n') $ pure (,) <*> (moveParser <* charP ' ') <*> moveParser

solve1 :: Repr1 -> Result
solve1 = sum . fmap roundPoints

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser1 solve1 [ (testData, 15) ]

part1 :: Solution Repr1 Result
part1 = Solution { filePathP="inputs/day2.txt"
                 , contentParser=fileParser1
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

strategyParser :: Parser Strategy
strategyParser = charMapP [('Z', Win), ('X', Lose), ('Y', Draw)]

fileParser2 :: Parser Repr2
fileParser2 = sepBy (charP '\n') playParser
  where playParser = pure (,) <*> (moveParser <* charP ' ') <*> strategyParser

useStrategy :: (Move, Strategy) -> (Move, Move)
useStrategy (m, Win) = (m, winningMove m)
useStrategy (m, Lose) = (m, losingMove m)
useStrategy (m, Draw) = (m, m)

solve2 :: Repr2 -> Result
solve2 = sum . fmap (roundPoints . useStrategy)

results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser2 solve2 [ (testData, 12) ]

part2 :: Solution Repr2 Result
part2 = part1 { contentParser=fileParser2
              , solveProblemP=solve2
              }
