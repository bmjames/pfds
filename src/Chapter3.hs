module Chapter3 where

import Data.Foldable hiding (foldr)
import Data.List (intersperse)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.TwoD.Layout.Tree as D

class HeapLike f where
    empty :: f a
    insert :: Ord a => a -> f a -> f a
    merge :: Ord a => f a -> f a -> f a
    findMin :: Ord a => f a -> Maybe a
    deleteMin :: Ord a => f a -> Maybe (f a)

type Rank = Int

data Heap a = Empty | Node Rank a (Heap a) (Heap a)
            deriving Show

rank :: Heap a -> Rank
rank Empty = 0
rank (Node r _ _ _) = r

unit :: a -> Heap a
unit x = Node 1 x Empty Empty

instance HeapLike Heap where
    empty = Empty

    insert x = merge (unit x)

    merge xs Empty = xs
    merge Empty ys = ys
    merge n1@(Node _ a1 l1 r1) n2@(Node _ a2 l2 r2)
        | a2 > a1   = let (l, r) = swap l1 (merge n2 r1) in Node (rank r + 1) a1 l r
        | otherwise = let (l, r) = swap l2 (merge n1 r2) in Node (rank r + 1) a2 l r
      where
        swap n1 n2 | rank n2 > rank n1 = (n2, n1)
                   | otherwise = (n1, n2)

    findMin Empty = Nothing
    findMin (Node _ a _ _) = Just a

    deleteMin Empty = Nothing
    deleteMin (Node _ _ l r) = Just (merge l r)

renderHeap :: (a -> String) -> Heap a -> Diagram B R2
renderHeap label = foldMap (D.renderTree renderNode renderEdge)
           . D.uniqueXLayout xsep ysep
           . toBTree

  where
    xsep = 2.0
    ysep = 2.75

    renderNode (s, a) = text (label a) # bold # font "sans-serif"
                     <> circle 1 # fc white

    renderEdge = (~~)

    toBTree Empty = D.Empty
    toBTree (Node s a l r) = D.BNode (s, a) (toBTree l) (toBTree r)

main :: IO ()
main = mainWith $ vcat
       $ intersperse (strut unitY)
       $ map (renderHeap return) trees
  where
    trees = scanl (flip insert) empty "happynewyear" :: [Heap Char]
