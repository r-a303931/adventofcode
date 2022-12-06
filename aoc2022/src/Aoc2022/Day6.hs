module Aoc2022.Day6 (results1, part1, results2, part2) where

import Control.Applicative
import Aoclibs.Lib
import Aoclibs.Parser
import Data.HashSet as HS

testData :: [String]
testData = [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
           , "bvwbjplbgvbhsrlpgdmjqwftvncz"
           , "nppdvjthqldpwncqszvftbrmjlhg"
           , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
           , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
           ]

firstMarker :: Int -> String -> Maybe Int
firstMarker = firstMarkerH 0
  where firstMarkerH i l xs | l == length (HS.fromList . fst $ splitAt l xs) = Just $ i + l
        firstMarkerH i l (_:xs) = firstMarkerH (i + 1) l xs
        firstMarkerH _ _ _ = Nothing

results1 :: Maybe [TestResult (Maybe Int)]
results1 = makeTestP (many item) (firstMarker 4) $ zip testData $ Just <$> [7,5,6,10,11]

part1 :: Solution String (Maybe Int) (Maybe Int)
part1 = Solution { filePathP="inputs/day6.txt"
                 , contentParser=many item
                 , solveProblemP=firstMarker 4
                 , displaySolutionP=id
                 }

results2 :: Maybe [TestResult (Maybe Int)]
results2 = makeTestP (many item) (firstMarker 14) $ zip testData $ Just <$> [19, 23, 23, 29, 26]

part2 :: Solution String (Maybe Int) (Maybe Int)
part2 = part1 { solveProblemP=firstMarker 14 }
