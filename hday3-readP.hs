-- AoC 2024, Day 3

-- solution using ReadP, same speed as
-- Data.Text + Text.Regex.TDFA since I rewrote it.
-- Finally I simplified the parsing but it is not
-- yet acceptable.

module Main where

import System.IO (readFile')
import Data.List (foldl')

--  modules for parsing
import Data.Char (isDigit)
import Data.Functor (($>))
import Control.Monad (void)
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,(<++)
  ,munch1
  ,many1
  ,char
  ,string
  ,get
  )

data Expr = Do |Dont |Val Int

showSolution :: String -> Int -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

getExprs :: String -> IO [Expr]
getExprs filename = parseDatas <$> readFile' filename

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

-- Parsing stuff
positive :: ReadP Int
positive = read <$> munch1 isDigit

-- I didn't find a way to get ReadP consume str
-- until end while parsing all Expr i.e.
-- do(), don't() and mul(n1,n2).
-- As last resort we take the last element of the
-- the results given by parseExprs since ReadP gives
-- us all results from smallest to biggest.

-- But there is a simpler solution than what I wrote
-- the first time, write 4 parsers:
--  - parseDo, parseDont, parseMul
--    and anyChar (just a (void get))
-- write a recursive parser that tries between these
-- 4 parsers (asum doesn't work here):
-- next = parseDo
--        <++ parseDont
--        <++ parseMul
--        <++ (anyChar *> next)
-- use: many1 next
-- to parse the input
-- Next time I'll use attoparsec.

parseDatas :: String -> [Expr]
parseDatas str =
  case  readP_to_S parseExprs str of
    xs@(_:_) -> fst (last xs)
    []  -> error "Error: parseDatas: can't parse."

-- The issue is, many1 isn't greedy and tries all solutions
parseExprs :: ReadP [Expr]
parseExprs = many1 next

next :: ReadP Expr
next = parseDo
        <++ parseDont
        <++ parseMul
        <++ (anyChar *> next)

anyChar :: ReadP ()
anyChar = void get

-- Hlint suggested to use ($>), so we tried.
parseDont :: ReadP Expr
parseDont = string "don't()" $> Dont

parseDo :: ReadP Expr
parseDo = string "do()" $> Do

parseMul :: ReadP Expr
parseMul = do
  void (string "mul(")
  n1 <- positive
  void (char ',')
  n2 <- positive
  void (char ')')
  pure (Val (n1 * n2))
