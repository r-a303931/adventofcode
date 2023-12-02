module Aoc2022.Day12 (results1, part1, results2, part2) where

import Control.Applicative
import Data.Char
import qualified Data.HashMap.Strict as HS
import Aoclibs.Lib
import Aoclibs.Parser
import qualified Aoclibs.Heap as H

data Elevation = Start | End | Height Int deriving Show

type Repr = [[(Elevation, Int, Int)]]
type Result = Int

testData :: String
testData = "Sabqponm\n\
           \abcryxxl\n\
           \accszExk\n\
           \acctuvwj\n\
           \abdefghi\n"

fileParser :: Parser Repr
fileParser =
  let
    elevationParser = (const Start <$> charP 'S') <|> (const End <$> charP 'E') <|> (Height . subtract 97 . ord <$> parseIf isLower)
    dataParser = sepBy (charP '\n') (many elevationParser)
    addRowIndices (i,row) = map (\(j,e) -> (e, i, j)) $ zip [0..] row
    addIndices table = map addRowIndices $ zip [0..] table
  in
    addIndices <$> dataParser

data Node = Node { nodeDist :: Int, nodePath :: [(Int, Int)], nodeCoords :: (Int, Int), nodeEDist :: Int } deriving Show

data Step = Step { stepDirection :: (Int, Int), stepNode :: Node } deriving Show

instance Ord Step where
  compare s1 s2 = if ()

data State = State { distMap :: HS.HashMap (Int, Int) Node, stepsToGo :: H.Heap Step }

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser undefined [ (testData, undefined) ]

part1 :: Solution Repr Result Result
part1 = Solution { filePathP="inputs/day12.text"
                 , contentParser=fileParser
                 , solveProblemP=undefined
                 , displaySolutionP=id
                 }

results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser undefined [ (testData, undefined) ]

part2 :: Solution Repr Result Result
part2 = Solution { filePathP="inputs/day12.text"
                 , contentParser=fileParser
                 , solveProblemP=undefined
                 , displaySolutionP=id
                 }
