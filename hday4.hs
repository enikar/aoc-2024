-- AoC 2024, Day 4

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Map.Strict qualified as M
import Data.Map.Strict ((!), Map)
import Text.Regex.TDFA ((=~))
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
  showSolution "Part2" (part2 datas)

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

part2 :: [Text] -> Int
part2 ts = foldl' f 0 [0..limit]
  where
    limit = length ts - 3
    lg  = T.length (head ts) - 3

    f acc n = acc + xMatchCount lg ls
      where ls = drop n ts

xMatchCount :: Int -> [Text] -> Int
xMatchCount limit ts = foldl' f 0 [0..limit]
  where
    f acc n
      | xMasCheck ls = acc + 1
      | otherwise    = acc
        where
          ls = map (T.drop n) ts

patterns :: Map Text Text
patterns = M.fromList
  [("M.M", "^S.S")
  ,("M.S", "^M.S")
  ,("S.M", "^S.M")
  ,("S.S", "^M.M")
  ]

xMasCheck :: [Text] -> Bool
xMasCheck ts = matchA && matchMS && matchSM
  where
    matchA = (ts !! 1) =~ "^.A."
    match1 = head ts =~ "^[MS].[MS]" :: Text
    matchMS = not (T.null match1)
    match1' = T.cons (T.head match1)
                      (T.snoc "." (T.last match1))

    pat = patterns ! match1'
    matchSM =  (ts !! 2) =~ pat
