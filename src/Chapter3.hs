module Chapter3 where

class HeapLike f where
    empty :: f a
    insert :: Ord a => a -> f a -> f a
    merge :: Ord a => f a -> f a -> f a

type Rank = Int

data Heap a = Empty | Node Rank a (Heap a) (Heap a)
            deriving Show

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
        merge' (Node s1 a1 l1 r1) n2@(Node s2 a2 l2 r2) = -- a2 is greater
            case l1 of Empty -> Node s1 a1 (merge n2 r1) Empty
                       Node s3 a3 l3 r3 | s3 < s1   -> Node (succ s3) a1 (merge n2 r1) l1
                                        | otherwise -> Node (succ s2) a1 l1 (merge n2 r1)

main :: IO ()
main = print (foldr insert empty ['y', 'x', 'a', 'f', 'b', 'h', 'c'] :: Heap Char)