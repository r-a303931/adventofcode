module Aoc2023.Lib (aocYear2023) where

import Aoclibs.Lib
import Aoclibs.Year
import qualified Aoc2023.Day1 as D1

storeProgram :: Show c => Solution a b c -> Maybe (IO ())
storeProgram = Just . runProgramP . (\p -> p { filePathP=("aoc2022/" ++ filePathP p) })

aocYear2023 :: AocYear
aocYear2023 = makeYear 2023 [ (storeProgram D1.part1, storeProgram D1.part2) ]
