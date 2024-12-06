-- AoC 2024, Day 4

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

default (T.Text)

readDatas :: String -> IO [Text]
readDatas filename = T.lines <$> TIO.readFile filename

showSolution :: String -> Int -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

main :: IO ()
main = do
  datas <- readDatas "day4.txt"
  showSolution "Part1" (part1 datas)
  --showSolution "Part2" (part2 datas)

numberOfMatches :: Text -> Int
numberOfMatches str = T.count xmas str + T.count samx str
  where
    xmas = "XMAS"
    samx = "SAMX"

part1 :: [Text] -> Int
part1 ls = sum (map ($ ls)
                    [sumOnLines
                    ,sumOnColumns
                    ,sumOnDiag1
                    ,sumOnDiag2
                    ])

sumOnLines :: [Text] -> Int
sumOnLines ls = sum (map numberOfMatches ls)

sumOnColumns :: [Text] -> Int
sumOnColumns ls = sumOnLines (T.transpose ls)

sumOnDiag1 :: [Text] -> Int
sumOnDiag1 ls =
  sumOnColumns
     (zipWith
       (\n line -> T.replicate n " " <> line)
       [0..]
       ls)

sumOnDiag2 :: [Text] -> Int
sumOnDiag2 ls = sumOnDiag1 (reverse ls)

-- TODO
-- part2 :: [Text] -> Int
-- part2 texts =
