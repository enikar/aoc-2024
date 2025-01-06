-- AoC 2024, Day 3
-- Finally I got a solution using Text.Regex.TDFA as fast
-- as the one using ReadP

{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.Maybe (fromMaybe)
import Data.Text.Read (decimal, signed)
import Data.List (foldl')
import Data.Array (Array, (!))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Text.Regex.TDFA (CompOption(..)
                       ,ExecOption(..)
                       ,Regex
                       ,makeRegexOpts
                       ,matchAllText
                       )
import Text.Regex.TDFA.Text ()

data Expr = Do |Dont |Val Int

compOpt :: CompOption
compOpt = CompOption {caseSensitive = True
                     ,multiline = False
                     ,rightAssoc = True
                     ,newSyntax = False
                     ,lastStarGreedy = False
                     }

execOpt :: ExecOption
execOpt = ExecOption { captureGroups = False }

showSolution :: String -> Int -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

getExprs :: String -> IO [Expr]
getExprs filename = parseExprs <$> TIO.readFile filename

main :: IO ()
main = do
  exprs <- getExprs "day3.txt"
  showSolution "Part1: " (part1 exprs)
  showSolution "Part2: " (part2 exprs)

part1 :: [Expr] -> Int
part1  = foldl' f 0
   where
     f acc (Val n) = acc + n
     f acc _       = acc

part2 :: [Expr] -> Int
part2 = fst . foldl' f (0, True)
  where
    f (n, _)          Do      = (n, True)
    f (n, _)          Dont    = (n, False)
    f  acc@(_, False) (Val _) = acc
    f (acc, True)     (Val n) = (acc+n, True)

-- parsing stuff
parseExprs :: Text -> [Expr]
parseExprs text = map f matches
  where
    matches = extractMatches (matchAllText regExpr text)

    f "don't()" = Dont
    f "do()"    = Do
    f x         = Val (product (extractNum x))

-- is there a better way to extract matches when using matchAllText?
extractMatches :: [Array Int (Text, (Int, Int))] -> [Text]
extractMatches = map (fst . (! 0))

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

regExpr :: Regex
regExpr = makeRegexOpts
              compOpt
              execOpt
              ("mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)|do\\(\\)|don't\\(\\)" :: Text)
