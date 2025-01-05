-- AoC 2024, Day 3
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Eta reduce" -}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text.Read (decimal, signed)
import Data.List (foldl')
import Data.Array (Array, (!))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Regex.TDFA (CompOption(..)
                       ,ExecOption(..)
                       ,Regex
                       ,makeRegexOpts
                       ,matchAllText
                       )
import Text.Regex.TDFA.Text ()

compOpt :: CompOption
compOpt = CompOption {caseSensitive = True
                     ,multiline = False
                     ,rightAssoc = True
                     ,newSyntax = False
                     ,lastStarGreedy = False
                     }

execOpt :: ExecOption
execOpt = ExecOption { captureGroups = False }

regPart1 :: Regex
regPart1 = makeRegexOpts
             compOpt
             execOpt
             ("mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)" :: Text)

showSolution :: String -> Int -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

main :: IO ()
main = do
  text <- T.concat . T.lines <$> TIO.readFile "day3.txt"
  showSolution "Part1: " (part1 text)
  showSolution "Part2: " (part2 text)

part1 :: Text -> Int
part1 text = sumProduct nums
  where
    nums = extractNums matches
    matches  = extractMatches (matchAllText regPart1 text)

-- is there a better way to extract matches when using matchAllText?
extractMatches :: [Array Int (Text, (Int, Int))] -> [Text]
extractMatches = map (fst . (! 0))

extractNums :: [Text] -> [[Int]]
extractNums = map extractNum

regXNum :: Regex
regXNum = makeRegexOpts
             compOpt
             execOpt
             ("[[:digit:]]{1,3}" :: Text)

extractNum :: Text -> [Int]
extractNum text = map
                  readInt
                  (extractMatches matches)
  where
    matches = matchAllText regXNum text


readInt :: Text -> Int
readInt = fromMaybe errRead . textReadMaybe
  where
    errRead = error "Error: readInt: not an Int"

textReadMaybe :: (Integral a, Read a) => Text -> Maybe a
textReadMaybe text = case signed decimal text of
                       Left _ -> Nothing
                       Right (x, _) -> Just x

regPart2 :: Regex
regPart2 = makeRegexOpts
              compOpt
              execOpt
              ("mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)|do\\(\\)|don't\\(\\)" :: Text)

part2 :: Text -> Int
part2 text = sumProduct nums
  where
    matches = extractMatches (matchAllText regPart2 text)
    nums = selectMul matches

selectMul :: [Text] -> [[Int]]
selectMul = snd . foldl' f (True, [])
  where
    f (_, muls)     "don't()" = (False, muls)
    f (_, muls)     "do()"    = (True, muls)
    f (False, muls) _         = (False, muls)
    f (True, muls)  x         = (True, extractNum x : muls)

sumProduct :: [[Int]] -> Int
sumProduct ls = foldl' f 0 ls
  where
    f acc nums = acc + product nums
