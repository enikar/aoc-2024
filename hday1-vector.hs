-- AoC 2024, Day 1

module Main where

import Data.Vector.Unboxed (fromList, Vector)
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Heap (sort)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

readPair :: String -> (Int, Int)
readPair s = (a, b)
  where
    errRead = error "Error: readPair: not an Int in day1.txt"
    readInt = fromMaybe errRead . readMaybe

    (left, right) = case words s of
      [le, ri] -> (le, ri)
      _        -> error "Error: readPair: can't read a pair of Int"

    a = readInt left
    b = readInt right

toSortedVector :: [Int] -> Vector Int
toSortedVector xs = runST $ do
  v <- V.unsafeThaw (fromList xs)
  sort v
  V.unsafeFreeze v


readDatas :: IO (Vector Int, Vector Int)
readDatas = do
  pairs <- map readPair . lines <$> readFile "day1.txt"
  let (left, right) = unzip pairs
      vleft = toSortedVector left
      vright = toSortedVector right
  pure (vleft, vright)


main :: IO ()
main = do
  (vleft, vright) <- readDatas
  showSolution "Part1" (part1 vleft vright)
  showSolution "Part2" (part2 vleft vright)

showSolution :: String -> Int -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1 :: Vector Int -> Vector Int -> Int
part1 vl vr = V.sum (V.zipWith f vl vr)
  where
    f x y = abs (x - y)

part2 :: Vector Int -> Vector Int -> Int
part2 vl vr = V.foldl' f 0 vl
  where
    f acc x = acc + x * V.length (V.filter (== x) vr)
    -- count y = V.foldl' g 0 vr
    --   where g acc z
    --           | y == z    = acc + 1
    --           | otherwise = acc
