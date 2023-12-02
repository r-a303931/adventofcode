module Aoc2022.Day13 (results1, part1) where

import Control.Applicative
import Data.Bool
import Data.Maybe
import Aoclibs.Lib
import Aoclibs.Parser

data Packet = PInt Int | PList [Packet]

instance Show Packet where
  show (PInt v) = show v
  show (PList ps) = show ps

type Repr = [(Packet, Packet)]
type Result = Int

testData :: String
testData = "[1,1,3,1,1]\n\
           \[1,1,5,1,1]\n\
           \\n\
           \[[1],[2,3,4]]\n\
           \[[1],4]\n\
           \\n\
           \[9]\n\
           \[[8,7,6]]\n\
           \\n\
           \[[4,4],4,4]\n\
           \[[4,4],4,4,4]\n\
           \\n\
           \[7,7,7,7]\n\
           \[7,7,7]\n\
           \\n\
           \[]\n\
           \[3]\n\
           \\n\
           \[[[]]]\n\
           \[[]]\n\
           \\n\
           \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
           \[1,[2,[3,[4,[5,6,0]]]],8,9]\n"

testData2 :: String
testData2 = "[[[[8,6]],[8,[7],[6,7,6,2,4]],10,[[1,7,9],7,[7,9]]],[[4,[],[10,5],[5,4,7],5],8,9,[[5,3,3,6,9],[9,5,10],8],[[0,6,9],[8],4,6,8]]]\n\
            \[[[],3,[[10,6,9,6],[6,8,7],[1,2]],8]]\n"

fileParser :: Parser Repr
fileParser = sepBy (charP '\n') ((,) <$> (packetParser <* charP '\n') <*> (packetParser <* charP '\n'))
  where packetParser = do
          let intParser = PInt <$> intP
          let listParser = (PList <$> (charP '[' *> sepBy (charP ',') packetParser <* charP ']')) <|> (const (PList []) <$> stringP "[]")
          intParser <|> listParser

inOrder :: (Packet, Packet) -> Bool
inOrder = fromMaybe False . inOrder'

inOrder' :: (Packet, Packet) -> Maybe Bool
inOrder' (PInt left, PInt right) | left == right = Nothing
inOrder' (PInt left, PInt right) = Just $ left < right
inOrder' (PList [], PList _) = Just True
inOrder' (PList _, PList []) = Just False
inOrder' (PInt left, PList right) = inOrder' (PList [PInt left], PList right)
inOrder' (PList left, PInt right) = inOrder' (PList left, PList [PInt right])
inOrder' (PList (l:ls), PList (r:rs)) = inOrder' (l, r) <|> inOrder' (PList ls, PList rs)

solve1 :: Repr -> Result
solve1 = sum . mapMaybe (\(i,ps) -> bool Nothing (Just i) $ inOrder ps) . zip [1..]

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser solve1 [ (testData, 13), (testData2, 0) ]

part1 :: Solution Repr Result Result
part1 = Solution { filePathP="inputs/day13.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }
