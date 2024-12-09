-- AoC 2024, Day 4

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Map.Strict qualified as M
import Data.Map.Strict ((!), Map)
import Text.Regex.TDFA (CompOption(..)
                       ,ExecOption(..)
                       ,Regex
                       ,makeRegexOpts
                       ,match
                       )

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

    matchCount = xMatchCount lg

    f acc n = acc + matchCount ls
      where ls = drop n ts

xMatchCount :: Int -> [Text] -> Int
xMatchCount limit ts = foldl' f 0 [0..limit]
  where
    f acc n
      | xMasCheck ls = acc + 1
      | otherwise    = acc
        where
          ls = map (T.drop n) ts

-- we compile regexes, it's 5 times faster than
-- my very first version
patterns :: Map Text Text
patterns = M.fromList
  [("MM", "^S.S")
  ,("MS", "^M.S")
  ,("SM", "^S.M")
  ,("SS", "^M.M")
  ]

compOpt :: CompOption
compOpt = CompOption {caseSensitive = True
                     ,multiline = True
                     ,rightAssoc = True
                     ,newSyntax = False
                     ,lastStarGreedy = False
                     }

execOpt :: ExecOption
execOpt = ExecOption { captureGroups = False }

lastLine :: Map Text Regex
lastLine = M.map (makeRegexOpts compOpt execOpt) patterns

firstLine :: Regex
firstLine = makeRegexOpts compOpt execOpt ("^[MS].[MS]" :: Text)

xMasCheck :: [Text] -> Bool
xMasCheck ts = matchA && matchMS && matchSM
  where
    matchA = T.index (ts !! 1) 1 == 'A'
    match1 = match firstLine (head ts) :: Text
    matchMS = not (T.null match1)
    match1' = T.pack [T.head match1, T.last match1]
    reg = lastLine ! match1'
    matchSM = match reg (ts !! 2)
