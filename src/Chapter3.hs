module Chapter3 where

class HeapLike f where
    empty :: f a
    insert :: Ord a => a -> f a -> f a
    merge :: Ord a => f a -> f a -> f a

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
    merge n1@(Node _ a1 _ _) n2@(Node _ a2 _ _) | a2 > a1   = merge' n1 n2
                                                | otherwise = merge' n2 n1
      where
        merge' (Node s1 a l1 r1) n | rank l1 < s1 = Node (succ (rank l1)) a (merge n r1) l1
                                   | otherwise    = Node (succ (rank n)) a l1 (merge n r1)

main :: IO ()
main = print (foldr insert empty ['y', 'x', 'a', 'f', 'b', 'h', 'c'] :: Heap Char)
