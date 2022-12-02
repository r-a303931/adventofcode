module Aoc2022.Lib (aocYear2022) where

import Aoclibs.Lib
import Aoclibs.Year
import qualified Aoc2022.Day1 as D1
import qualified Aoc2022.Day2 as D2

storeProgram :: Show c => Solution a b c -> Maybe (IO ())
storeProgram = Just . runProgramP . (\p -> p { filePathP=("aoc2022/" ++ filePathP p) })

aocYear2022 :: AocYear
aocYear2022 = makeYear 2022 [(storeProgram D1.part1, storeProgram D1.part2), (storeProgram D2.part1, storeProgram D2.part2)]
