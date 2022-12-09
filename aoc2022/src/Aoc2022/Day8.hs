module Aoc2022.Day8 (results1, results2, part1, part2) where

import Control.Applicative
import Data.Char
import Data.List (singleton, transpose)

import Aoclibs.Lib
import Aoclibs.Parser

type Repr = [[Int]]
type Result = Int

testData :: String
testData = "30373\n\
           \25512\n\
           \65332\n\
           \33549\n\
           \35390\n"

testArr :: Repr
testArr = [[3,0,3,7,3],[2,5,5,1,2],[6,5,3,3,2],[3,3,5,4,9],[3,5,3,9,0]]

fileParser :: Parser Repr
fileParser = sepBy (charP '\n') (some (read . singleton <$> parseIf isDigit))

maximumE :: [Int] -> Int
maximumE [] = -1
maximumE e = maximum e

colVals :: [[Int]] -> Int -> Int -> ([Int], [Int])
colVals forest r n = let (firstRows,lastRows) = tail <$> splitAt r forest in (map (!!n) firstRows, map (!!n) lastRows)

calculateVisibility :: Repr -> Int
calculateVisibility forest = let forest' = zip [0..] forest in sum $ map (calculateVals 0 (-1)) forest'
  where calculateVals _ _ (_,[]) = 0
        calculateVals ind highest (r,(v:vs)) =
          let (above,below) = colVals forest r ind
          in calculateVals (ind + 1) (max highest v) (r,vs) + if v > highest || v > maximumE vs || v > maximumE above || v > maximumE below then 1 else 0

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser calculateVisibility [ (testData, 21) ]

part1 :: Solution Repr Result Result
part1 = Solution { filePathP="inputs/day8.txt"
                 , contentParser=fileParser
                 , solveProblemP=calculateVisibility
                 , displaySolutionP=id
                 }

calculateVisibility2 :: Repr -> Int
calculateVisibility2 forest = let forest' = zip [0..] forest in maximum $ map (maximum . calculateViews 0 []) forest'
  where calculateViews _ _ (_,[]) = []
        calculateViews c p (r,(v:vs)) =
          let (above,below) = colVals forest r c
          in ((untilHigher v p) * (untilHigher v vs) * (untilHigher v (reverse above)) * (untilHigher v (below))) : calculateViews (c + 1) (v : p) (r,vs)
        untilHigher v [] = 0
        untilHigher v (t:_) | t >= v = 1
        untilHigher v (_:ts) = 1 + untilHigher v ts

results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser calculateVisibility2 [ (testData, 8) ]

part2 :: Solution Repr Result Result
part2 = part1 { solveProblemP=calculateVisibility2 }
