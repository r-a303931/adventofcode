module Aoc2022.Day3 (results1, part1, results2, part2) where

import Aoclibs.Lib
import Aoclibs.Parser
import Data.Char
import qualified Data.HashSet as HS

type Result = Int
type Repr = [[String]]

testData :: String
testData = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
           \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
           \PmmdzqPrVvPwwTWBwg\n\
           \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
           \ttgJtRGJQctTZtZT\n\
           \CrZsJsPPZsGzwwsLwLmpwMDw\n"

fileParser1 :: Parser Repr
fileParser1 = fmap split' <$> sepBy (charP '\n') (spanP isAlpha)
  where split' r = let (s1, s2) = splitAt (length r `div` 2) r in [s1, s2]

itemPriority :: Char -> Int
itemPriority c | isUpper c = ord c - 38
itemPriority c             = ord c - 96

solve :: Repr -> Result
solve = sum . fmap solveRow
  where solveRow group = sum . fmap itemPriority . HS.toList . foldr HS.intersection (HS.fromList $ head group) $ HS.fromList <$> tail group

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser1 solve [ (testData, 157) ]

part1 :: Solution Repr Result Int
part1 = Solution { filePathP="inputs/day3.txt"
                 , contentParser=fileParser1
                 , solveProblemP=solve
                 , displaySolutionP=id
                 }

fileParser2 :: Parser Repr
fileParser2 = sepBy (charP '\n') $ b3 <$> (lineParse <* charP '\n') <*> (lineParse <* charP '\n') <*> lineParse
  where lineParse = (\x -> [x]) <$> spanP isAlpha
        b3 a b c = a ++ b ++ c

results2 = makeTestP fileParser2 solve [ (testData, 70) ]

results :: Maybe [TestResult Result]
results = (++) <$> results1 <*> results2

part2 :: Solution Repr Result Int
part2 = part1 { contentParser=fileParser2 }
