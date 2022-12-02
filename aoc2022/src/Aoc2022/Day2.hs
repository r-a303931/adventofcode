module Aoc2022.Day2 where

import Control.Applicative
import Aoclibs.Lib
import Aoclibs.Parser

data Move = Rock | Paper | Scissors deriving Show
data Strategy = Win | Lose | Draw deriving Show
type Repr1 = [(Move, Move)]
type Repr2 = [(Move, Strategy)]
type Result = Int

testData :: String
testData = "A Y\n\
           \B X\n\
           \C Z\n"

itBeats :: Move -> Move -> Bool
itBeats Rock Scissors = True
itBeats Scissors Paper = True
itBeats Paper Rock = True
itBeats _ _ = False

points :: Move -> Int
points Rock = 1
points Paper = 2
points Scissors = 3

roundPoints :: (Move, Move) -> Int
roundPoints (them, me) | itBeats me them = 6 + points me
roundPoints (them, me) | itBeats them me = 0 + points me
roundPoints (_,    me)                 = 3 + points me

moveParser :: Parser Move
moveParser = rockParser <|> paperParser <|> scissorsParser
  where rockParser = const Rock <$> (charP 'A' <|> charP 'X')
        paperParser = const Paper <$> (charP 'B' <|> charP 'Y')
        scissorsParser = const Scissors <$> (charP 'C' <|> charP 'Z')

fileParser1 :: Parser Repr1
fileParser1 = sepBy (charP '\n') $ pure (,) <*> (moveParser <* charP ' ') <*> moveParser

solve1 :: Repr1 -> Result
solve1 = sum . fmap roundPoints

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser1 solve1 [ (testData, 15) ]

part1 = Solution { filePathP="inputs/day2.txt"
                 , contentParser=fileParser1
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

beats :: Move -> Move
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

loses :: Move -> Move
loses Rock = Scissors
loses Paper = Rock
loses Scissors = Paper

strategyParser :: Parser Strategy
strategyParser = winParser <|> loseParser <|> drawParser
  where winParser = const Win <$> charP 'Z'
        loseParser = const Lose <$> charP 'X'
        drawParser = const Draw <$> charP 'Y'

fileParser2 :: Parser Repr2
fileParser2 = sepBy (charP '\n') playParser
  where playParser = pure (,) <*> (moveParser <* charP ' ') <*> strategyParser

useStrategy :: (Move, Strategy) -> (Move, Move)
useStrategy (m, Win) = (m, beats m)
useStrategy (m, Lose) = (m, loses m)
useStrategy (m, Draw) = (m, m)

solve2 :: Repr2 -> Result
solve2 = sum . fmap (roundPoints . useStrategy)

results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser2 solve2 [ (testData, 12) ]

part2 = part1 { contentParser=fileParser2
              , solveProblemP=solve2
              }
