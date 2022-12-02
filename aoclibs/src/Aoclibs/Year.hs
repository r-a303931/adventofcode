module Aoclibs.Year (AocYear, makeYear, year, yearSolutions) where

data AocYear = AocYear { year :: Int, yearSolutions :: [(Maybe (IO ()), Maybe (IO ()))] }

makeYear :: Int -> [(Maybe (IO ()), Maybe (IO ()))] -> AocYear
makeYear = AocYear
