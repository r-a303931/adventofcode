module Aoc2022.Day5 (results1, part1, results2, part2) where

import Control.Applicative
import Data.List (transpose)
import Data.Maybe
import Data.Tuple
import Aoclibs.Lib
import Aoclibs.Parser

type Repr = ([[Char]], [(Int, Int, Int)])
type Result = [[Char]]

testData :: String
testData = "    [D]    \n\
           \[N] [C]    \n\
           \[Z] [M] [P]\n\
           \ 1   2   3 \n\
           \\n\
           \move 1 from 2 to 1\n\
           \move 3 from 1 to 3\n\
           \move 2 from 2 to 1\n\
           \move 1 from 1 to 2\n"

inputParser :: Parser Repr
inputParser = do
  let boxParser = charP '[' *> item <* charP ']'
  let lineParser = sepBy (charP ' ') ((Just <$> boxParser) <|> (const Nothing <$> stringP "   "))
  boxes <- sepBy (charP '\n') lineParser
  _ <- charP '\n'
  _ <- sepBy (charP ' ') (charP ' ' *> intP <* charP ' ')
  _ <- stringP "\n\n"
  let moveParser = (,,) <$> (stringP "move " *> intP) <*> (stringP " from " *> ((subtract 1) <$> intP)) <*> (stringP " to " *> ((subtract 1) <$> intP))
  moves <- sepBy (charP '\n') moveParser

  return (map catMaybes . transpose $ boxes, moves)

executeMoves :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
executeMoves crates []               = crates
executeMoves crates ((0,_,_):moves') = executeMoves crates moves'
executeMoves crates ((c,f,t):moves') =
  let
    (poppedCrates, crateToMove) = modifyArray (\(x:xs) -> (xs, x)) f crates
    addedCrates = fst $ modifyArray (\xs -> (crateToMove:xs, ())) t poppedCrates
  in
    executeMoves addedCrates ((c - 1,f,t):moves')

results1 :: Maybe [TestResult Result]
results1 = makeTestP inputParser (uncurry executeMoves) [ (testData, ["C", "M", "ZNDP"]) ]

part1 :: Solution Repr Result String
part1 = Solution { filePathP="inputs/day5.txt"
                 , contentParser=inputParser
                 , solveProblemP=uncurry executeMoves
                 , displaySolutionP=map head
                 }

executeMoves2 :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
executeMoves2 crates []               = crates
executeMoves2 crates ((c,f,t):moves') =
  let
    (poppedCrates, cratesToMove) = modifyArray (\cr -> let (moved, left) = splitAt c cr in (left, moved)) f crates
    addedCrates = fst $ modifyArray (\xs -> (cratesToMove ++ xs, ())) t poppedCrates
  in
    executeMoves2 addedCrates moves'

results2 :: Maybe [TestResult Result]
results2 = makeTestP inputParser (uncurry executeMoves2) [ (testData, ["M", "C", "DNZP"]) ]

part2 :: Solution Repr Result String
part2 = part1 { solveProblemP=uncurry executeMoves2 }
