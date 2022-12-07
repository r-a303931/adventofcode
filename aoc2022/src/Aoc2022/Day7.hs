module Aoc2022.Day7 (results1, part1, results2, part2) where

import Control.Applicative
import Control.Monad
import Data.List (intercalate, sort)
import Data.Maybe
import Data.Tuple

import Aoclibs.Lib
import Aoclibs.Parser

data FileRepr = FDir String | FFile (String, Int) deriving Show
data IntermediateRepr = Cd String | Ls [FileRepr] deriving Show
data Repr = RDir (String, [Repr]) | RFile (String, Int) deriving Show

testData :: String
testData = "$ cd /\n\
           \$ ls\n\
           \dir a\n\
           \14848514 b.txt\n\
           \8504156 c.dat\n\
           \dir d\n\
           \$ cd a\n\
           \$ ls\n\
           \dir e\n\
           \29116 f\n\
           \2557 g\n\
           \62596 h.lst\n\
           \$ cd e\n\
           \$ ls\n\
           \584 i\n\
           \$ cd ..\n\
           \$ cd ..\n\
           \$ cd d\n\
           \$ ls\n\
           \4060174 j\n\
           \8033020 d.log\n\
           \5626152 d.ext\n\
           \7214296 k\n"


fileParser :: Parser Repr
fileParser = reduceTree <$> many cmdParser
   where cmdParser = cdParser <|> lsParser
         cdParser = Cd <$> (stringP "$ cd " *> spanP ((/=) '\n') <* charP '\n')
         lsParser = Ls <$> (stringP "$ ls\n" *> many fReprParser)
         fReprParser = (FDir <$> fDirParser) <|> (FFile . swap <$> fFileParser)
         fDirParser = stringP "dir " *> spanP ((/=) '\n') <* charP '\n'
         fFileParser = (,) <$> (intP <* charP ' ') <*> spanP ((/=) '\n') <* charP '\n'

findAndReplace :: (a -> Bool) -> (a -> a) -> [a] -> [a]
findAndReplace p f [] = []
findAndReplace p f (a:as) | p a = f a : as
findAndReplace p f (a:as) = a : findAndReplace p f as

reduceTree :: [IntermediateRepr] -> Repr
reduceTree = reduceTreeH "" $ RDir ("", [])
  where reduceTreeH :: String -> Repr -> [IntermediateRepr] -> Repr
        reduceTreeH cwd fs ((Cd ".."):cmds)    = reduceTreeH (intercalate "/" . init . wordsWhen (== '/') $ cwd) fs cmds
        reduceTreeH _   fs ((Cd ('/':p)):cmds) = reduceTreeH p fs cmds
        reduceTreeH cwd fs ((Cd p):cmds)       = reduceTreeH (cwd ++ "/" ++ p) fs cmds
        reduceTreeH _   fs []                  = fs
        reduceTreeH cwd fs ((Ls files):cmds)   = reduceTreeH cwd (fromMaybe fs (insertFiles (wordsWhen (== '/') cwd) fs (transformRepr <$> files))) cmds
        transformRepr (FDir n)  = RDir (n, [])
        transformRepr (FFile d) = RFile d
        insertFiles :: [String] -> Repr -> [Repr] -> Maybe Repr
        insertFiles (d:ds) (RDir (n,fs)) files =
          let
            folderWithName n1 (RDir (n2, _)) = n1 == n2
            folderWithName _  _              = False
            updateFolder (RDir (n2,fs')) = fromMaybe (RDir (n2, fs')) $ insertFiles ds (RDir(n2,fs')) files
          in
            Just $ RDir (n, findAndReplace (folderWithName d) updateFolder fs)
        insertFiles _      (RFile _)     _     = Nothing
        insertFiles []     (RDir (n,_))  files = Just $ RDir (n, files)

treeSize :: Repr -> Int
treeSize (RFile (_, s)) = s
treeSize (RDir (_, f)) = sum . map treeSize $ f

visitDirs :: (String -> [Repr] -> a) -> Repr -> [a]
visitDirs _ (RFile _) = []
visitDirs f (RDir (n, fs)) = f n fs : (join $ visitDirs f <$> fs)

solve :: Repr -> [Int]
solve = visitDirs (\n fs -> treeSize $ RDir (n, fs))

solve1 :: Repr -> Int
solve1 = sum . filter (\d -> d < 100000) . solve

results1 :: Maybe [TestResult Int]
results1 = makeTestP fileParser solve1 [ (testData, 95437) ]

part1 :: Solution Repr Int Int
part1 = Solution { filePathP="inputs/day7.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve1
                 , displaySolutionP=id
                 }

solve2      :: Repr -> Int
solve2 repr = head . filter (\d -> (treeSize repr) - d <= 40000000) . sort . solve $ repr

results2 :: Maybe [TestResult Int]
results2 = makeTestP fileParser solve2 [ (testData, 24933642) ]

part2 :: Solution Repr Int Int
part2 = part1 { solveProblemP=solve2 }
