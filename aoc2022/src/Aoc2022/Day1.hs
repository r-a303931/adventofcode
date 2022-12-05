module Aoc2022.Day1 (results1, part1, results2, part2) where

import Control.Applicative
import Data.List (sort)
import Aoclibs.Lib
import Aoclibs.Parser

type Result = Int
type Repr = [[Int]]

testData :: String
testData = "1000\n\
           \2000\n\
           \3000\n\
           \\n\
           \4000\n\
           \\n\
           \5000\n\
           \6000\n\
           \\n\
           \7000\n\
           \8000\n\
           \9000\n\
           \\n\
           \10000\n\
           \"

fileParser :: Parser [[Int]]
fileParser = sepBy (charP '\n') (some (intP <* charP '\n'))

solve1 :: Repr -> Result
solve1 = maximum . fmap sum

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser solve1 [ (testData, 24000) ]

part1 :: Solution Repr Result Int
part1 = Solution { filePathP="inputs/day1.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

solve2 :: Repr -> Result
solve2 = sum . foldl keepTop3 [0,0,0]
  where keepTop3 top3 new = take 3 $ reverse $ sort $ (sum new) : top3

results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser solve2 [ (testData, 45000) ]

part2 :: Solution Repr Result Int
part2 = part1 { solveProblemP=solve2 }
