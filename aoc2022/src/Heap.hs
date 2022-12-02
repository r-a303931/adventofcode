module Heap where

import Control.Applicative
import Data.Bool
import Data.List

data HeapNode a = HeapNode
  { heapLeftChild :: Maybe (HeapNode a)
  , heapRightChild :: Maybe (HeapNode a)
  , heapNodeValue :: a
  }

data HeapBase a = HeapBase { heapRoot :: HeapNode a, heapNodeCountBase :: Int }
data Heap a = Heap (Maybe (HeapBase a))

showHelp :: Show a => [Bool] -> Bool -> Int -> HeapNode a -> [String]
showHelp s l d n =
  let
    pv = show $ heapNodeValue n
    rs = if length s > 0 then init s else []
    ob = (intercalate "" . fmap (bool "   " "│  ") $ rs)
    lb = if d > 0 then if l then "├─ " else "└─ " else ""
    pc = case (heapLeftChild n, heapRightChild n) of
      (Just lc, Just rc) ->
        showHelp (s ++ [True]) True (d + 1) lc ++ showHelp (s ++ [False]) False (d + 1) rc

      (Just lc, Nothing) ->
        showHelp (s ++ [False]) False (d + 1) lc

      (Nothing, Just rc) ->
        ["Huh? Right: "] ++ (showHelp (s ++ [False]) False (d + 1) rc)

      _ -> []
  in
    [ob ++ lb ++ pv] ++ pc

heapBase :: Heap a -> Maybe (HeapBase a)
heapBase (Heap b) = b

instance Show a => Show (Heap a) where
  show = maybe "(empty)" (intercalate "\n" . showHelp [] False 0 . heapRoot) . heapBase

instance Show a => Show (HeapNode a) where
  show = intercalate "\n" . showHelp [] False 0

instance Show a => Show (HeapBase a) where
  show = show . heapRoot

emptyHeap :: Heap a
emptyHeap = Heap Nothing

heapEmptyNode :: a -> HeapNode a
heapEmptyNode v = HeapNode { heapNodeValue=v, heapLeftChild=Nothing, heapRightChild=Nothing }

heapPeek :: Heap a -> Maybe a
heapPeek = fmap (heapNodeValue . heapRoot) . heapBase

heapHeight :: Heap a -> Int
heapHeight = maybe 0 (\h -> floor $ log (fromIntegral . heapNodeCountBase $ h) / (log 2 :: Double)) . heapBase

heapNodeCount :: Heap a -> Int
heapNodeCount = maybe 0 heapNodeCountBase . heapBase

heapIsNull :: Heap a -> Bool
heapIsNull = null . heapBase

heapIsLeaf :: HeapNode a -> Bool
heapIsLeaf n = null (heapLeftChild n <|> heapRightChild n)

heapIsIndexToTheRight :: Int -> Bool
heapIsIndexToTheRight 2 = False
heapIsIndexToTheRight 3 = True
heapIsIndexToTheRight n = heapIsIndexToTheRight (n `div` 2)

heapIsIndexRightOf :: Int -> Int -> Bool
heapIsIndexRightOf 1 x = heapIsIndexToTheRight x
heapIsIndexRightOf x x'
  | x == x' `div` 2 = x' `rem` 2 == 1
  | otherwise = heapIsIndexRightOf x (x' `div` 2)

heapPop :: (Ord a, Show a) => Heap a -> Maybe (a, Heap a)
heapPop = fmap f . heapBase
  where heapRemoveLast n c i =
          case (heapLeftChild n, heapRightChild n, heapIsIndexRightOf c i) of
            (Just v, _, False) ->
              let (res, v') = heapRemoveLast v (c * 2) i in (res, Just $ n { heapLeftChild=v' })

            (_, Just v, True) ->
              let (res, v') = heapRemoveLast v (c * 2 + 1) i in (res, Just $ n { heapRightChild=v' })

            _ -> (n, Nothing)

        heapifyDown n =
          case (heapLeftChild n, heapRightChild n) of
            (Nothing, _) ->
              n

            (Just lc, Nothing)
              | heapNodeValue lc < heapNodeValue n ->
                lc { heapRightChild=heapRightChild n , heapLeftChild=Just . heapifyDown $ n { heapRightChild=heapRightChild lc , heapLeftChild=heapLeftChild lc } }
              | otherwise -> n

            (Just lc, Just rc)
              | heapNodeValue lc < heapNodeValue n && heapNodeValue lc < heapNodeValue rc ->
                lc { heapRightChild=heapRightChild n , heapLeftChild=Just . heapifyDown $ n { heapRightChild=heapRightChild lc , heapLeftChild=heapLeftChild lc } }
              | heapNodeValue rc < heapNodeValue n ->
                rc { heapLeftChild=heapLeftChild n , heapRightChild=Just . heapifyDown $ n { heapLeftChild=heapLeftChild rc , heapRightChild=heapRightChild rc } }
              | otherwise -> n
        f v =
          let
            (lastChild, poppedRoot) = heapRemoveLast (heapRoot v) 1 (heapNodeCountBase v)
            updateRoot n = heapifyDown $ lastChild { heapLeftChild=heapLeftChild n, heapRightChild=heapRightChild n }
            createNewHeap n = HeapBase { heapRoot=updateRoot n, heapNodeCountBase=(heapNodeCountBase v) - 1 }
          in
            (heapNodeValue . heapRoot $ v, Heap $ createNewHeap <$> poppedRoot)

heapInsert :: (Ord a) => Heap a -> a -> Heap a
heapInsert (Heap Nothing) v =
  let
    root = HeapNode { heapNodeValue=v , heapRightChild=Nothing , heapLeftChild=Nothing }
  in
    Heap . Just $ HeapBase { heapRoot=root, heapNodeCountBase=1 }
heapInsert (Heap (Just b)) v =
  let
    newNode = HeapNode { heapNodeValue=v, heapLeftChild=Nothing, heapRightChild=Nothing }
    addLast n c i =
      case (c == i, heapLeftChild n, heapRightChild n, heapIsIndexRightOf c i) of
        (True, _, _, _) -> n

        (_, Nothing, _, False) ->
          if heapNodeValue n > v
            then
              let n' = n { heapRightChild=Nothing, heapLeftChild=Nothing } in newNode { heapRightChild=heapRightChild n , heapLeftChild=Just n' }
            else
              n { heapLeftChild=Just newNode }


        (_, _, Nothing, True) ->
          if heapNodeValue n > v
            then
              let n' = n { heapRightChild=Nothing, heapLeftChild=Nothing } in newNode { heapLeftChild=heapLeftChild n , heapRightChild=Just n' }
            else
              n { heapRightChild=Just newNode }

        (_, Just v', _, False) ->
          let
            v'' = addLast v' (c * 2) i
            n' = n { heapLeftChild=Just v'' }
          in
            if heapNodeValue v'' < heapNodeValue n'
              then v'' { heapLeftChild=Just $ n' { heapLeftChild=heapLeftChild v'', heapRightChild=heapRightChild v'' }, heapRightChild=heapRightChild n' }
              else n'

        (_, _, Just v', True) ->
          let
            v'' = addLast v' (c * 2 + 1) i
            n' = n { heapRightChild=Just v'' }
          in
            if heapNodeValue v'' < heapNodeValue n'
              then v'' { heapRightChild=Just $ n' { heapRightChild=heapRightChild v'', heapLeftChild=heapLeftChild v'' }, heapLeftChild=heapLeftChild n' }
              else n'
  in
    Heap . Just $ HeapBase
      { heapRoot=addLast (heapRoot b) 1 (heapNodeCountBase b + 1)
      , heapNodeCountBase=heapNodeCountBase b + 1
      }

heapFromList :: (Ord a) => [a] -> Heap a
heapFromList = foldl heapInsert emptyHeap
