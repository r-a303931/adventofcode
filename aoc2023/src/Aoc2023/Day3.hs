module Aoc2023.Day3 where

import Aoclibs.Lib
import Aoclibs.Parser

type Result = Int
data Numbers = Numbers { numVal :: Int
                       , numX :: Int
                       , numY :: Int
                       }
type Repr = ()

testData :: String
testData =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598..\n\
  \"
