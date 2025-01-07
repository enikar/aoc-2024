-- AoC 2024, Day3
-- This time we are using attoparsec.
-- it is very fast.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.List (foldl')

-- modules for parsing
import Data.Either (fromRight)
import Data.Functor (void)
import Control.Applicative (asum)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Attoparsec.Text
  (Parser
  ,parseOnly
  ,decimal
  ,string
  ,char
  ,anyChar
  ,many1
  )


data Expr = Do |Dont |Val Int

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
-- inspired by https://entropicthoughts.com/parser-combinators-beat-regexes
parseExprs :: Text -> [Expr]
parseExprs text =
  fromRight (error "Failed to parse input")
            (parseOnly expressions text)
  where
    expressions :: Parser [Expr]
    expressions = many1 next

    next :: Parser Expr
    next = asum
      [parseDo
      ,parseDont
      ,parseMul
      ,anyChar *> next]

    parseDo :: Parser Expr
    parseDo = do
      void (string "do()")
      pure Do

    parseDont :: Parser Expr
    parseDont = do
      void (string "don't()")
      pure Dont

    parseMul :: Parser Expr
    parseMul = do
      void (string "mul(")
      n1 <- decimal
      void (char ',')
      n2 <- decimal
      void (char ')')
      pure (Val (n1 * n2))
