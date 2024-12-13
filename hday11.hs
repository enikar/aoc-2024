-- AoC 2024, Day 11
{-# LANGUAGE ImportQualifiedPost #-}
{- HLINT ignore "Eta reduce" -}

module Main (main) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Bits (shiftR)
import Data.IntMap.Strict qualified as M
import Data.IntMap.Strict (IntMap)

import Data.List (foldl')

readInt :: String -> Int
readInt s = fromMaybe errRead  (readMaybe s)
  where
    errRead = error ("Error: readInt: not an Int: " <> s)

initialMap :: [Int] -> IntMap Int
initialMap = foldl' f M.empty
  where
    f acc x = M.alter (inc 1) x acc

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  datas <- map readInt . words <$> readFile "day11.txt"
  let stones = initialMap datas
  printSolution "Part1" (countStones (replicateBlink 25 stones))
  printSolution "Part2" (countStones (replicateBlink 75 stones))

countStones :: IntMap Int -> Int
countStones = M.foldl' (+) 0

replicateBlink :: Int -> IntMap Int -> IntMap Int
replicateBlink blinks stones = snd (until satisfy improve (0, stones))
  where
    satisfy (n, _) = n == blinks
    improve (n, stones') = (n+1, blink stones')

blink :: IntMap Int -> IntMap Int
blink = M.foldlWithKey' update M.empty

update :: IntMap Int -> Int -> Int -> IntMap Int
update stones key n = foldl' g stones next
  where
    next = applyRules key
    g acc y = M.alter (inc n) y acc

inc :: Int -> Maybe Int -> Maybe Int
inc n Nothing = Just n
inc n (Just m) = Just (m+n)

applyRules :: Int -> [Int]
applyRules n
  | n == 0      = [1]
  | n1 == 0     = [n * 2024]
  | otherwise   = [n1, n2]
     where (n1, n2) = splitIfEvenDigits n

splitIfEvenDigits :: Int -> (Int, Int)
splitIfEvenDigits n
  | even dn    = (n1, n2)
  | otherwise  = (0, n)
  where
    dn = digitNumber n
    half = shiftR dn 1 -- divide by 2
    (n1, n2) = n `quotRem` (10^half)

digitNumber :: Int -> Int
digitNumber n = snd (until satisfy  improve (n, 0))
  where
    satisfy (n', _) = n' == 0
    improve (n', k) = (n' `quot` 10, k+1)
