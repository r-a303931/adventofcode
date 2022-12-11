module Aoc2022.Day11 (results1, part1, results2, part2) where

import Control.Applicative
import Data.Bool
import Data.List (sortOn)
import Data.Maybe
import Aoclibs.Lib
import Aoclibs.Parser

data Monkey = Monkey { monkeyNo :: Int, mItems :: [Integer], mOperation :: Operation, mDivisor :: Int, mThrowTest :: Integer -> Int, mInspections :: Integer }

type Repr = [Monkey]
type Result = Integer

instance Show Monkey where
  show monkey = "Monkey " ++ show (monkeyNo monkey) ++ ": " ++ show (mItems monkey) ++ "; inspections: " ++ show (mInspections monkey)

data OperationType = OMultiply | OAdd deriving Show
data Operation = Operation { oFirst :: Maybe Integer, oType :: OperationType, oSecond :: Maybe Integer }

instance Show Operation where
  show (Operation opFirst OMultiply opSecond) = (maybe "old" show opFirst) ++ " * " ++ (maybe "old" show opSecond)
  show (Operation opFirst OAdd opSecond) = (maybe "old" show opFirst) ++ " + " ++ (maybe "old" show opSecond)

evalOperation :: Operation -> Integer -> Integer
evalOperation (Operation opFirst OMultiply opSecond) input = fromMaybe input opFirst * fromMaybe input opSecond
evalOperation (Operation opFirst OAdd opSecond) input = fromMaybe input opFirst + fromMaybe input opSecond

testData :: String
testData = "Monkey 0:\n\
           \  Starting items: 79, 98\n\
           \  Operation: new = old * 19\n\
           \  Test: divisible by 23\n\
           \    If true: throw to monkey 2\n\
           \    If false: throw to monkey 3\n\
           \\n\
           \Monkey 1:\n\
           \  Starting items: 54, 65, 75, 74\n\
           \  Operation: new = old + 6\n\
           \  Test: divisible by 19\n\
           \    If true: throw to monkey 2\n\
           \    If false: throw to monkey 0\n\
           \\n\
           \Monkey 2:\n\
           \  Starting items: 79, 60, 97\n\
           \  Operation: new = old * old\n\
           \  Test: divisible by 13\n\
           \    If true: throw to monkey 1\n\
           \    If false: throw to monkey 3\n\
           \\n\
           \Monkey 3:\n\
           \  Starting items: 74\n\
           \  Operation: new = old + 3\n\
           \  Test: divisible by 17\n\
           \    If true: throw to monkey 0\n\
           \    If false: throw to monkey 1\n"

fileParser :: Parser Repr
fileParser = sepBy (charP '\n') monkeyParser
  where monkeyParser = do
          no <- stringP "Monkey " *> intP <* stringP ":\n  Starting items: "
          firstItems <- sepBy (stringP ", ") intP
          let opP = (Just . toInteger <$> intP) <|> (const Nothing <$> stringP "old")
          opFirst <- stringP "\n  Operation: new = " *> opP
          opType <- charP ' ' *> charMapP [('*', OMultiply), ('+', OAdd)] <* charP ' '
          opSecond <- opP <* stringP "\n  Test: divisible by "
          mThrowTestDivisorVal <- toInteger <$> intP
          mTrueResult <- stringP "\n    If true: throw to monkey " *> intP <* charP '\n'
          mFalseResult <- stringP "    If false: throw to monkey " *> intP <* charP '\n'

          return $ Monkey { monkeyNo=no
                          , mItems=toInteger <$> firstItems
                          , mOperation=Operation { oFirst=opFirst, oType=opType, oSecond=opSecond }
                          , mThrowTest=(\x -> bool mFalseResult mTrueResult $ x `mod` mThrowTestDivisorVal == 0)
                          , mInspections=0
                          , mDivisor=fromInteger mThrowTestDivisorVal
                          }

type ManageWorry = (Monkey -> Integer -> Integer)

doMonkey :: ManageWorry -> Repr -> Int -> Repr
doMonkey _ monkeys mid | (length . mItems $ monkeys !! mid) == 0 = monkeys
doMonkey management monkeys mid =
  let
    monkey = monkeys !! mid
    mitem = head . mItems $ monkey
    newLevel = management monkey $ evalOperation (mOperation monkey) mitem
    targetMonkey = mThrowTest monkey newLevel
    newItems = tail . mItems $ monkey

    updateMonkeys (i,m) = if i == targetMonkey
                             then (i, m { mItems=mItems m ++ [newLevel] })
                             else if i == mid
                                     then (i, m { mItems=newItems, mInspections=mInspections m + 1 })
                                     else (i, m)
    newMonkeys = map (snd . updateMonkeys) $ zip [0..] monkeys
  in
    doMonkey management newMonkeys mid

doRound :: ManageWorry -> Repr -> Repr
doRound management monkeys = foldl (doMonkey management) monkeys [0..length monkeys - 1]

doRounds :: ManageWorry -> Int -> Repr -> Repr
doRounds _ 0 monkeys = monkeys
doRounds management r monkeys = doRounds management (r - 1) $ doRound management monkeys

monkeyBusiness :: [Monkey] -> Maybe Result
monkeyBusiness (m1:m2:_) = Just $ mInspections m1 * mInspections m2
monkeyBusiness _ = Nothing

monkeys' = fst $ fromJust $ parse fileParser testData

solve :: (Repr -> ManageWorry) -> Int -> Repr -> Maybe Result
solve management rounds repr = monkeyBusiness . reverse . sortOn mInspections . doRounds (management repr) rounds $ repr

results1 :: Maybe [TestResult (Maybe Result)]
results1 = makeTestP fileParser (solve (\_ _ x -> x `div` 3) 20) [ (testData, Just 10605) ]


part1 :: Solution Repr (Maybe Result) (Maybe Result)
part1 = Solution { filePathP="inputs/day11.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve (\_ _ x -> x `div` 3) 20
                 , displaySolutionP=id
                 }

getCommonDivisor :: Repr -> Int
getCommonDivisor = product . map mDivisor

results2 :: Maybe [TestResult (Maybe Result)]
results2 = makeTestP fileParser (solve (\ms _ x -> x `mod` (toInteger $ getCommonDivisor ms)) 10000) [ (testData, Just 2713310158) ]

part2 :: Solution Repr (Maybe Result) (Maybe Result)
part2 = Solution { filePathP="inputs/day11.txt"
                 , contentParser=fileParser
                 , solveProblemP=solve (\ms _ x -> x `mod` (toInteger $ getCommonDivisor ms)) 10000
                 , displaySolutionP=id
                 }
