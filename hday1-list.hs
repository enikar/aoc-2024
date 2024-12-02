-- AoC 2024, Day 1

module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List (sort, foldl')

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


readDatas :: IO ([Int], [Int])
readDatas = do
  pairs <- map readPair . lines <$> readFile "day1.txt"
  let (left, right) = unzip pairs
  pure (sort left, sort right)


main :: IO ()
main = do
  (left, right) <- readDatas
  showSolution "Part1" (part1 left right)
  showSolution "Part2" (part2 left right)

showSolution :: String -> Int -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1 :: [Int] -> [Int] -> Int
part1 vl vr = sum (zipWith f vl vr)
  where
    f x y = abs (x - y)

part2 :: [Int] -> [Int] -> Int
part2 vl vr = foldl' f 0 vl
  where
    f acc x = acc + x * length (filter (==x) vr)
