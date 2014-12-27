{-# LANGUAGE ScopedTypeVariables, NPlusKPatterns #-}

-- | Chapter 2 - Persistence
module Chapter2 where

import Control.Applicative hiding (empty)
import Control.Monad.Trans.Cont
import Data.Maybe (isJust)
import Prelude hiding (elem, lookup)
import Test.QuickCheck
import qualified Data.List as L


-- Exercise 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs@(_:xs') = xs : suffixes xs'


class SetLike f where
    fromList :: Ord a => [a] -> f a
    fromList = foldr insert empty

    empty :: f a
    insert :: Ord a => a -> f a -> f a
    member :: Ord a => a -> f a -> Bool

data Tree a = Empty | Node a (Tree a) (Tree a)
            deriving Show

instance SetLike Tree where
    empty = Empty

    -- Exercise 2.2
    -- (implement member so it makes no more than depth + 1 comparisons)
    member a = go Nothing where
        go (Just c) Empty = a == c
        go Nothing Empty  = False
        go c (Node a' ls rs) | a <= a' = go (Just a') ls
                             | otherwise = go c rs

    -- Exercise 2.3
    -- (implement insert so that inserting an existing element does no copying)
    insert elem root = flip runCont id $ callCC $ \abort ->
        let go Empty = return $ Node elem Empty Empty
            go (Node a ls rs) | elem < a = Node a <$> go ls <*> pure rs
                              | elem > a = Node a ls <$> go rs
                              | otherwise = abort root
        in go root

-- Exercise 2.5

-- | A complete binary tree
complete :: a      -- ^ element to fill each node
         -> Int    -- ^ depth of tree
         -> Tree a
complete a = go where
    go 0 = empty
    go d = let t = go (d - 1) in Node a t t

-- | A balanced (but possibly incomplete) binary tree
balanced :: a      -- ^ element to fill each node
         -> Int    -- ^ number of elements
         -> Tree a
balanced a = go where
    go 0     = empty
    go (n+1) | isEven n  = Node a t t
             | otherwise = Node a t t'
      where
        m = n `div` 2
        t = go m
        t' = go (m + 1)

    isEven n = n `mod` 2 == 0


-- Exercise 2.6

class MapLike f where
    listToMap :: Ord k => [(k, v)] -> f k v
    listToMap = foldr bind emptyMap

    emptyMap :: f k v
    bind :: Ord k => (k, v) -> f k v -> f k v
    lookup :: Ord k => k -> f k v -> Maybe v

data TreeMap k v = EmptyMap | MapNode k v (TreeMap k v) (TreeMap k v)
                 deriving Show

instance MapLike TreeMap where
    emptyMap = EmptyMap

    bind (k, v) = go where
        go EmptyMap = MapNode k v EmptyMap EmptyMap
        go (MapNode k' v' ls rs) | k < k' = MapNode k' v' (go ls) rs
                                 | k > k' = MapNode k' v' ls (go rs)
                                 | otherwise = MapNode k v ls rs

    lookup k = go Nothing where
        go (Just (k', v)) EmptyMap | k == k' = Just v
        go _              EmptyMap = Nothing
        go c (MapNode k' v ls rs) | k <= k' = go (Just (k, v)) ls
                                  | otherwise = go c rs

main :: IO ()
main = do
    quickCheck $ \(xs :: [Int]) -> L.tails xs == suffixes xs
    quickCheck $ \xs -> all (`member` (fromList xs :: Tree Int)) xs
    quickCheck $ \xs -> all (\(k, _) -> isJust $ lookup k (listToMap xs :: TreeMap Int Int)) xs
