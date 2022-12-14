module Aoc2022.Lib (aocYear2022) where

import Aoclibs.Lib
import Aoclibs.Year
import qualified Aoc2022.Day1 as D1
import qualified Aoc2022.Day2 as D2
import qualified Aoc2022.Day3 as D3
import qualified Aoc2022.Day4 as D4
import qualified Aoc2022.Day5 as D5
import qualified Aoc2022.Day6 as D6
import qualified Aoc2022.Day7 as D7
import qualified Aoc2022.Day8 as D8
import qualified Aoc2022.Day9 as D9
import qualified Aoc2022.Day10 as D10
import qualified Aoc2022.Day11 as D11

storeProgram :: Show c => Solution a b c -> Maybe (IO ())
storeProgram = Just . runProgramP . (\p -> p { filePathP=("aoc2022/" ++ filePathP p) })

aocYear2022 :: AocYear
aocYear2022 = makeYear 2022 [ (storeProgram D1.part1, storeProgram D1.part2)
                            , (storeProgram D2.part1, storeProgram D2.part2)
                            , (storeProgram D3.part1, storeProgram D3.part2)
                            , (storeProgram D4.part1, storeProgram D4.part2)
                            , (storeProgram D5.part1, storeProgram D5.part2)
                            , (storeProgram D6.part1, storeProgram D6.part2)
                            , (storeProgram D7.part1, storeProgram D7.part2)
                            , (storeProgram D8.part1, storeProgram D8.part2)
                            , (storeProgram D9.part1, storeProgram D9.part2)
                            , (storeProgram D10.part1, storeProgram D10.part2)
                            , (storeProgram D11.part1, storeProgram D11.part2)
                            ]
