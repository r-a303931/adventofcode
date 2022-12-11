module Aoc2022.Day10 (results1, part1, part2) where

import Control.Applicative
import Aoclibs.Lib
import Aoclibs.Parser

data Instruction = INoop | IAddx Int deriving Show

type Repr = [Instruction]
type Result = Int
type Result2 = String

testData :: String
testData = "addx 15\n\
           \addx -11\n\
           \addx 6\n\
           \addx -3\n\
           \addx 5\n\
           \addx -1\n\
           \addx -8\n\
           \addx 13\n\
           \addx 4\n\
           \noop\n\
           \addx -1\n\
           \addx 5\n\
           \addx -1\n\
           \addx 5\n\
           \addx -1\n\
           \addx 5\n\
           \addx -1\n\
           \addx 5\n\
           \addx -1\n\
           \addx -35\n\
           \addx 1\n\
           \addx 24\n\
           \addx -19\n\
           \addx 1\n\
           \addx 16\n\
           \addx -11\n\
           \noop\n\
           \noop\n\
           \addx 21\n\
           \addx -15\n\
           \noop\n\
           \noop\n\
           \addx -3\n\
           \addx 9\n\
           \addx 1\n\
           \addx -3\n\
           \addx 8\n\
           \addx 1\n\
           \addx 5\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \addx -36\n\
           \noop\n\
           \addx 1\n\
           \addx 7\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \addx 2\n\
           \addx 6\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \addx 1\n\
           \noop\n\
           \noop\n\
           \addx 7\n\
           \addx 1\n\
           \noop\n\
           \addx -13\n\
           \addx 13\n\
           \addx 7\n\
           \noop\n\
           \addx 1\n\
           \addx -33\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \addx 2\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \addx 8\n\
           \noop\n\
           \addx -1\n\
           \addx 2\n\
           \addx 1\n\
           \noop\n\
           \addx 17\n\
           \addx -9\n\
           \addx 1\n\
           \addx 1\n\
           \addx -3\n\
           \addx 11\n\
           \noop\n\
           \noop\n\
           \addx 1\n\
           \noop\n\
           \addx 1\n\
           \noop\n\
           \noop\n\
           \addx -13\n\
           \addx -19\n\
           \addx 1\n\
           \addx 3\n\
           \addx 26\n\
           \addx -30\n\
           \addx 12\n\
           \addx -1\n\
           \addx 3\n\
           \addx 1\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \addx -9\n\
           \addx 18\n\
           \addx 1\n\
           \addx 2\n\
           \noop\n\
           \noop\n\
           \addx 9\n\
           \noop\n\
           \noop\n\
           \noop\n\
           \addx -1\n\
           \addx 2\n\
           \addx -37\n\
           \addx 1\n\
           \addx 3\n\
           \noop\n\
           \addx 15\n\
           \addx -21\n\
           \addx 22\n\
           \addx -6\n\
           \addx 1\n\
           \noop\n\
           \addx 2\n\
           \addx 1\n\
           \noop\n\
           \addx -10\n\
           \noop\n\
           \noop\n\
           \addx 20\n\
           \addx 1\n\
           \addx 2\n\
           \addx 2\n\
           \addx -6\n\
           \addx -11\n\
           \noop\n\
           \noop\n\
           \noop\n"

fileParser :: Parser Repr
fileParser = sepBy (charP '\n') commandParser
  where commandParser = (const INoop <$> stringP "noop") <|> (IAddx <$> (stringP "addx " *> eitherInt))
        eitherInt = intP <|> ((* (-1)) <$> (charP '-' *> intP))

signalStrengths :: Int -> Repr -> [Int]
signalStrengths _ [] = []
signalStrengths x (INoop:ins) = x : signalStrengths x ins
signalStrengths x ((IAddx nx):ins) = x : x : signalStrengths (x + nx) ins

solve1 :: Repr -> Result
solve1 repr = sum . map (uncurry (*)) . filter (\(i,_) -> (i - 20) `mod` 40 == 0) $ (zip [1..] $ signalStrengths 1 repr)

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser solve1 [ (testData, 13140) ]

part1 :: Solution Repr Result Result
part1 = Solution { filePathP="inputs/day10.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

renderStrengths :: Int -> [Int] -> String
renderStrengths _ [] = ""
renderStrengths c (s:ss) = (if elem (c-1) [s - 1, s, s + 1] then "#" else ".") ++ (if c `mod` 40 == 0 then "\n" else "") ++ renderStrengths ((c `mod` 40) + 1) ss

part2 :: Solution Repr String String
part2 = Solution { filePathP="inputs/day10.txt"
                 , contentParser=fileParser
                 , solveProblemP=renderStrengths 1 . signalStrengths 1
                 , displaySolutionP=id
                 }
