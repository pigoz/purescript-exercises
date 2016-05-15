module Main where

import Prelude
import Data.Array.Unsafe (head, tail)
import Data.Array (filter, (..))
import Control.MonadPlus (guard)

isEven :: Int -> Boolean
isEven 1 = false
isEven 2 = true
isEven n = isEven (n - 2)

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count f a =
  cur + count f t
  where
    h = head a
    t = tail a
    cur = if f h then 1 else 0

countEven :: Array Int -> Int
countEven = count isEven

squares :: Array Int -> Array Int
squares = map (\n -> n * n)

filterNegative :: Array Int -> Array Int
filterNegative = filter (\n -> n >= 0)

infixl 9 filter as <$?>

filterNegative' :: Array Int -> Array Int
filterNegative' a = (\n -> n >= 0) <$?> a

cartesian :: forall a. Array a -> Array a -> Array (Array a)
cartesian x y = do
  i <- x
  j <- y
  return [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ a * a + b * b == c * c
  return [a, b, c]
