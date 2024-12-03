-- AoC 2024 Day 2
module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

readDatas :: IO [[Int]]
readDatas = do
  ls <- lines <$> readFile "day2.txt"
  let errRead = error "Error: readDatas: not an Int in day2.txt"
      readInt = fromMaybe errRead . readMaybe
  pure (map (map readInt . words) ls)


main :: IO ()
main = do
  datas <- readDatas
  showSolution "Part1" (partx part1 datas)
  showSolution "Part2" (partx part2 datas)

showSolution :: String -> Int -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)


partx :: ([Int] -> Bool) -> [[Int]] -> Int
partx p = length . filter p

part1 :: [Int] -> Bool
part1 ls = all (between inf sup) diffs
  where
    diffs = computeDiffs ls
    sign = signum (head diffs)
    (inf, sup) = if sign == 1 then (1, 3) else (-3, -1)


computeDiffs :: [Int] -> [Int]
computeDiffs ls = zipWith (-) (drop 1 ls) ls

between :: Int -> Int -> Int -> Bool
between inf sup x = inf <= x && x <= sup

removeAt :: [Int] -> Int -> [Int]
removeAt xs index = take index xs ++ drop (index+1) xs

part2 :: [Int] -> Bool
part2 ls = part1 ls || part2' ls

part2' :: [Int] -> Bool
part2' ls = foldr (f . removeAt ls) False [0 .. maxindex]
  where
    maxindex = length ls - 1

    f _ True = True
    f xs False = all (between inf sup) ds
      where
        ds = computeDiffs xs
        sign = signum (head ds)
        (inf, sup) = if sign == 1 then (1, 3) else (-3, -1)
