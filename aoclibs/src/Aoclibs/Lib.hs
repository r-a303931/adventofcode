module Aoclibs.Lib (TestResult, Solution(..), ShowLines(..), mTrace, makeTestP, run, runProgramP, wordsWhen, modifyArray) where

import Data.Maybe
import Debug.Trace
import Aoclibs.Parser

mTrace :: (a -> String) -> a -> a
mTrace = (trace =<<)

data TestResult a = Success | Failure (a, a)
  deriving (Show)

testCase          :: Eq b => (a -> b) -> (a, b) -> TestResult b
testCase f (a, b) = if res == b then Success else Failure (res, b)
  where res = f a

makeTestT :: Eq b => (a -> b) -> [(a, b)] -> [TestResult b]
makeTestT = map . testCase

makeTestP       :: Eq b => Parser a -> (a -> b) -> [(String, b)] -> Maybe [TestResult b]
makeTestP p f d = test <$> parsed
  where test   = makeTestT f
        parsed = sequenceA . map (\(i,o) -> (flip (,) o . fst) <$> (parse p i)) $ d

data Solution a b c = Solution { filePathP :: String
                               , contentParser :: Parser a
                               , solveProblemP :: a -> b
                               , displaySolutionP :: b -> c
                               }

runProgramP         :: Show c => Solution a b c -> IO ()
runProgramP program = interactFile (filePathP program) $ fromJust . (\s -> ((displaySolutionP program) . (solveProblemP program) . fst) <$> (parse (contentParser program) s))

run         :: Show c => Solution a b c -> IO ()
run program = const () <$> sequenceA [runProgramP program, putStrLn ""]

interactFile      :: Show a => String -> (String -> a) -> IO ()
interactFile fp f = (show . f <$> readFile fp) >>= putStr

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


modifyArray :: (a -> (a, b)) -> Int -> [a] -> ([a], b)
modifyArray f ind arr = let (a, b) = f $ arr !! ind in (take ind arr ++ [a] ++ drop (ind + 1) arr, b)

data ShowLines a = ShowLines [a]

instance Show a => Show (ShowLines a) where
  show (ShowLines a) = unlines . map show $ a
