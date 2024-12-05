-- AoC 2024, Day 3
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text.Read (decimal, signed)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA ((=~)
                       ,getAllTextSubmatches
                       ,getAllTextMatches
                       )
default (T.Text)

showSolution :: String -> Int -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

main :: IO ()
main = do
  text <- TIO.readFile "day3.txt"
  showSolution "Part1: " (part1 text)
  showSolution "Part2: " (part2 text)

part1 :: T.Text -> Int
part1 text = sum (map product datas)
  where
    regex = "mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)"
    datas = extractNums (getAllTextMatches (text =~ regex) :: [T.Text])

extractNums :: [T.Text] -> [[Int]]
extractNums = map extractNum

extractNum :: T.Text -> [Int]
extractNum text = map
                  readInt
                  (drop 1 (getAllTextSubmatches (text =~ regex) :: [T.Text]))
  where
    regex = "([[:digit:]]{1,3}),([[:digit:]]{1,3})"


readInt :: T.Text -> Int
readInt = fromMaybe errRead . textReadMaybe
  where
    errRead = error "Error: readInt: not an Int"

textReadMaybe :: (Integral a, Read a) => T.Text -> Maybe a
textReadMaybe text = case signed decimal text of
                       Left _ -> Nothing
                       Right (x, _) -> Just x

part2 :: T.Text -> Int
part2 text = sum (map product datas)
  where
    datas = selectMul (getAllTextMatches (text =~ regex) :: [T.Text])
    regex = "mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)|do\\(\\)|don't\\(\\)"

selectMul :: [T.Text] -> [[Int]]
selectMul texts = snd (foldl' f (True, []) texts)
  where
    f (_, muls)     "don't()" = (False, muls)
    f (_, muls)     "do()"    = (True, muls)
    f (False, muls) _         = (False, muls)
    f (True, muls)  x         = (True, extractNum x : muls)
