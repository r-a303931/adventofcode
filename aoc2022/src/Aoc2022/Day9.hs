module Aoc2022.Day9 where

import Control.Monad

import qualified Data.HashSet as HS

import Aoclibs.Lib
import Aoclibs.Parser

data Move = MLeft | MRight | MUp | MDown deriving (Show, Eq)

type Repr = [Move]
type Result = Int

testData :: String
testData = "R 4\n\
           \U 4\n\
           \L 3\n\
           \D 1\n\
           \R 4\n\
           \D 1\n\
           \L 5\n\
           \R 2\n"

fileParser :: Parser Repr
fileParser = join <$> sepBy (charP '\n') moveParser
  where moveParser = do
          move <- charMapP [('L', MLeft), ('R', MRight), ('U', MUp), ('D', MDown)]
          count <- charP ' ' *> intP
          return $ take count $ repeat move

solve1 :: Repr -> Result
solve1 = length . (\(x,_,_) -> x) . foldl solve1Inner (HS.empty, (0,0), (0,0))

moveHead :: Move -> (Int, Int) -> (Int, Int)
moveHead MLeft (x,y) = (x - 1, y)
moveHead MRight (x,y) = (x + 1, y)
moveHead MUp (x,y) = (x, y - 1)
moveHead MDown (x,y) = (x, y + 1)

moveTailToHead :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTailToHead (hx,hy) (tx,ty) | (elem tx [hx - 1, hx, hx + 1]) && (elem ty [hy - 1, hy, hy + 1]) = (tx, ty)
moveTailToHead (hx,hy) (tx,ty) | tx > hx && ty == hy = (tx - 1, ty)
moveTailToHead (hx,hy) (tx,ty) | tx < hx && ty == hy = (tx + 1, ty)
moveTailToHead (hx,hy) (tx,ty) | ty > hy && tx == hx = (tx, ty - 1)
moveTailToHead (hx,hy) (tx,ty) | ty < hy && tx == hx = (tx, ty + 1)
moveTailToHead (hx,hy) (tx,ty) | tx > hx && ty > hy = (tx - 1, ty - 1)
moveTailToHead (hx,hy) (tx,ty) | tx < hx && ty > hy = (tx + 1, ty - 1)
moveTailToHead (hx,hy) (tx,ty) | tx > hx && ty < hy = (tx - 1, ty + 1)
moveTailToHead (hx,hy) (tx,ty) | tx < hx && ty < hy = (tx + 1, ty + 1)

solve1Inner :: (HS.HashSet (Int, Int), (Int, Int), (Int, Int)) -> Move -> (HS.HashSet (Int, Int), (Int, Int), (Int, Int))
solve1Inner (moves, rHead, rTail) move =
  let
    newHead = moveHead move rHead
    newTail = moveTailToHead newHead rTail
  in
    (HS.insert newTail moves, newHead, newTail)

results1 :: Maybe [TestResult Result]
results1 = makeTestP fileParser solve1 [ (testData, 13) ]

part1 :: Solution Repr Result Result
part1 = Solution { filePathP="inputs/day9.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

testData2 :: String
testData2 = "R 5\n\
            \U 8\n\
            \L 8\n\
            \D 3\n\
            \R 17\n\
            \D 10\n\
            \L 25\n\
            \U 20\n"

solve2 :: Repr -> Result
solve2 = length . (\(x,_,_) -> x) . foldl solve2Inner (HS.empty, (0,0), take 9 $ repeat (0,0))

updateTails :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
updateTails _ [] = []
updateTails prevLink (t:ts) = moveTailToHead prevLink t : updateTails (moveTailToHead prevLink t) ts

solve2Inner :: (HS.HashSet (Int, Int), (Int, Int), [(Int, Int)]) -> Move -> (HS.HashSet (Int, Int), (Int, Int), [(Int, Int)])
solve2Inner (moves, rHead, rTails) move =
  let
    newHead = moveHead move rHead
    newTail = updateTails newHead rTails
  in
    (HS.insert (last newTail) moves, newHead, newTail)

results2 :: Maybe [TestResult Result]
results2 = makeTestP fileParser solve2 [ (testData, 1), (testData2, 36) ]


part2 :: Solution Repr Result Result
part2 = Solution { filePathP="inputs/day9.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve2
                 , displaySolutionP=id
                 }
