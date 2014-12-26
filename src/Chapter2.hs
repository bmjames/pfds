{-# LANGUAGE ScopedTypeVariables #-}

-- | Chapter 2 - Persistence
module Chapter2 where

import Control.Applicative hiding (empty)
import Control.Monad.Trans.Cont
import Test.QuickCheck
import qualified Data.List as L


-- Exercise 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs@(_:xs') = xs : suffixes xs'


data Tree a = Empty | Node a (Tree a) (Tree a)
            deriving Show

class SetLike f where
    fromList :: Ord a => [a] -> f a
    fromList = foldr insert empty

    empty :: f a
    insert :: Ord a => a -> f a -> f a
    member :: Ord a => a -> f a -> Bool

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

main :: IO ()
main = do
    quickCheck $ \(xs :: [Int]) -> L.tails xs == suffixes xs
    quickCheck $ \xs -> all (`member` (fromList xs :: Tree Int)) xs
