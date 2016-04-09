module SelectSort where

import Control.Monad.Sharing
import Data.Monadic.List
import Data.List (delete)
type Cmp alpha mu = alpha -> alpha -> mu Bool

minM :: (Monad mu, Sharing mu) => Cmp (mu Int) mu -> mu Int -> mu Int -> mu Int
minM p x y = do
  x' <- share x
  y' <- share y
  b <- p x' y'
  if b then x' else y'

minM' :: (Monad mu, Sharing mu) => Cmp Int mu -> Int -> Int -> mu Int
minM' p x y = do
  b <- p x y
  return (if b then x else y)

minimumM :: (Monad mu, Sharing mu) => Cmp (mu Int) mu -> mu (List mu Int) -> mu Int
minimumM p list = do
  Cons x xs <- list
  xs' <- xs
  case xs' of
   Nil -> x
   Cons _ _ -> do
     y <- share (minimumM p (return xs'))
     minM p x y

minimumM' :: (Monad mu, Sharing mu) => Cmp Int mu -> mu (List mu Int) -> mu Int
minimumM' p list = do
  Cons x xs <- list
  xs' <- xs
  case xs' of
   Nil -> x
   Cons _ _ -> do
     y <- share (minimumM' p (return xs'))
     -- y >>= \y' -> x >>= \x' -> minM' p x' y'
     y >>= \y' -> x >>= \x' -> do
       b <- p x' y'
       if b then return x' else return y'

-- selectSortM  :: (MonadPlus mu, Sharing mu, Shareable mu Int)
--              => Cmp Int mu -> mu (List mu Int) -> mu (List mu Int)
-- selectSortM p xs = do
--   xs' <- xs
--   case xs' of
--    Nil -> nil
--    Cons _ _ -> do
--      x <- share (minimumM p xs')
--      ys <- share (delete' x xs')
--      cons x (selectSortM p ys)

coinCmp :: MonadPlus mu => alpha -> alpha -> mu Bool
coinCmp _ _ = return True `mplus` return False

main1 = do
  result <- resultList (minimumM coinCmp (convert [(1::Int)..3]) >>= convert)
  mapM_ print (result :: [Int])

main2 = do
  result <- resultList (minimumM' coinCmp (convert [(1::Int)..3]) >>= convert)
  mapM_ print (result :: [Int])


-- main2 = do
--   result <- resultList (selectSortM coinCmp (convert [(1::Int)..3]) >>= convert)
--   mapM_ print (result :: [[Int]])

-- delete' :: (Eq alpha, Monad mu) => mu alpha -> List mu alpha -> mu (List mu alpha)
-- delete' _ Nil = nil
-- delete' x (Cons y ys) = do
--   x' <- x
--   y' <- y
--   ys' <- share ys
--   if x' == y'
--    then delete' x ys'
--    else cons y (delete' x ys')

