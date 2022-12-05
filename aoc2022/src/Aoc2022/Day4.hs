module Aoc2022.Day4 (results1, part1, results2, part2) where

import Aoclibs.Lib
import Aoclibs.Parser

type Result = Int
type Repr = [((Int, Int), (Int, Int))]

testData :: String
testData = "2-4,6-8\n\
           \2-3,4-5\n\
           \5-7,7-9\n\
           \2-8,3-7\n\
           \6-6,4-6\n\
           \2-6,4-8\n"

fileParser :: Parser Repr
fileParser = sepBy (charP '\n') (lineParser)
  where rangeParser = (,) <$> (intP <* charP '-') <*> intP
        lineParser = (,) <$> (rangeParser <* charP ',') <*> rangeParser

solve1 :: Repr -> Result
solve1 = length . filter (uncurry rangesOverlap)
  where rangesOverlap r1 r2 = contains r1 r2 || contains r2 r1
        contains (s1,e1) (s2,e2) = s1 <= s2 && e1 >= e2


results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser solve1 [ (testData, 2) ]

part1 :: Solution Repr Result Int
part1 = Solution { filePathP="inputs/day4.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

solve2 :: Repr -> Result
solve2 = length . filter (uncurry rangesOverlap)
  where rangesOverlap (s1,e1) (s2,e2) = s1 >= s2 && s1 <= e2 || e1 >= s2 && e1 <= e2 || s2 >= s1 && s2 <= e1 || e2 >= s1 && e2 <= e1


results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser solve2 [ (testData, 4) ]

part2 :: Solution Repr Result Int
part2 = part1 { solveProblemP=solve2 }
