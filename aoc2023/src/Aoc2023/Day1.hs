module Aoc2023.Day1 (results1, part1, results2, part2) where

import Aoclibs.Lib
import Aoclibs.Parser
import Data.Char (isDigit)
import Data.List (isPrefixOf)

type Result = Int

type Repr = [(Char, Char)]

testData :: String
testData =
  "1abc2\n\
  \pqr3stu8vwx\n\
  \a1b2c3d4e5f\n\
  \treb7uchet\n\
  \"

testData2 :: String
testData2 =
  "two1nine\n\
  \eightwothree\n\
  \abcone2threexyz\n\
  \xtwone3four\n\
  \4nineeightseven2\n\
  \zoneight234\n\
  \7pqrstsixteen\n\
  \"

findDigit :: String -> Char
findDigit (d : _) | isDigit d = d
findDigit (_ : ds) = findDigit ds
findDigit [] = undefined

fileParser :: Parser Repr
fileParser = linesP lineParser
  where
    lineParser = do
      line <- tokenP
      let firstDigit = findDigit line
      let lastDigit = findDigit . reverse $ line
      return (firstDigit, lastDigit)

solve :: Repr -> Result
solve = sum . map (read . \(a, b) -> [a, b])

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser solve [(testData, 142)]

part1 :: Solution Repr Result Int
part1 =
  Solution
    { filePathP = "inputs/day1.txt",
      contentParser = fileParser,
      solveProblemP = solve,
      displaySolutionP = id
    }

findDigit2 :: String -> Char
findDigit2 [] = undefined
findDigit2 ds | "one" `isPrefixOf` ds = '1'
findDigit2 ds | "two" `isPrefixOf` ds = '2'
findDigit2 ds | "three" `isPrefixOf` ds = '3'
findDigit2 ds | "four" `isPrefixOf` ds = '4'
findDigit2 ds | "five" `isPrefixOf` ds = '5'
findDigit2 ds | "six" `isPrefixOf` ds = '6'
findDigit2 ds | "seven" `isPrefixOf` ds = '7'
findDigit2 ds | "eight" `isPrefixOf` ds = '8'
findDigit2 ds | "nine" `isPrefixOf` ds = '9'
findDigit2 ds | "zero" `isPrefixOf` ds = '0'
findDigit2 (d : _) | isDigit d = d
findDigit2 (_ : ds) = findDigit2 ds

findDigit3 :: String -> Char
findDigit3 [] = undefined
findDigit3 ds | "eno" `isPrefixOf` ds = '1'
findDigit3 ds | "owt" `isPrefixOf` ds = '2'
findDigit3 ds | "eerht" `isPrefixOf` ds = '3'
findDigit3 ds | "ruof" `isPrefixOf` ds = '4'
findDigit3 ds | "evif" `isPrefixOf` ds = '5'
findDigit3 ds | "xis" `isPrefixOf` ds = '6'
findDigit3 ds | "neves" `isPrefixOf` ds = '7'
findDigit3 ds | "thgie" `isPrefixOf` ds = '8'
findDigit3 ds | "enin" `isPrefixOf` ds = '9'
findDigit3 ds | "orez" `isPrefixOf` ds = '0'
findDigit3 (d : _) | isDigit d = d
findDigit3 (_ : ds) = findDigit3 ds

fileParser2 :: Parser Repr
fileParser2 = linesP lineParser
  where
    lineParser = do
      line <- tokenP
      let firstDigit = findDigit2 line
      let lastDigit = findDigit3 . reverse $ line
      return (firstDigit, lastDigit)

results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser2 solve [(testData2, 281)]

part2 :: Solution Repr Result Int
part2 = part1 {contentParser = fileParser2}
