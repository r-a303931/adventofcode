module Main where

import qualified Data.List as L
import qualified Text.Read as T

import qualified Aoclibs.Year as Y
import Aoc2022.Lib
import Aoc2023.Lib

years :: [Y.AocYear]
years = [aocYear2022, aocYear2023]

getYear :: [Y.AocYear] -> IO Y.AocYear
getYear years' = do
  putStrLn "Available years:"
  putStr $ unlines $ show . Y.year <$> years'
  putStrLn "Select a year:"

  inp <- T.readMaybe <$> getLine

  maybe (getYear years') return $ inp >>= (\v -> L.find ((== v) . Y.year) years')

showDay :: (Int, (Maybe a, Maybe a)) -> Bool
showDay (_, (Nothing, Nothing)) = False
showDay _ = True

getDay :: Y.AocYear -> IO ((Maybe (IO ()), Maybe (IO ())))
getDay year = do
  putStrLn "Available days:"
  let days = filter showDay $ zip [1..] (Y.yearSolutions year)
  putStr $ unlines $ (show . fst) <$> days
  putStrLn "Select a day:"

  inp <- T.readMaybe <$> getLine

  maybe (getDay year) (return . snd) $ inp >>= (\v -> L.find ((== v) . fst) days)

getPart :: (Maybe (IO ()), Maybe (IO ())) -> IO (IO ())
getPart parts = case parts of
  (Just p, Nothing) -> return p

  (Nothing, Just p) -> return p

  (Just p1, Just p2) -> do
    putStrLn "Part 1 or 2?"
    inp <- getLine

    case inp of
      "1" -> return p1
      "2" -> return p2

      _ -> getPart parts

main :: IO ()
main = do
  year <- getYear years
  day <- getDay year
  part <- getPart day

  part

  putStrLn "\n"

  main
