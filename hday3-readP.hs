-- AoC 2024, Day 3

-- solution using ReadP, same speed as
-- Data.Text + Text.Regex.TDFA since I rewrote it.
-- We should improve the parsing, but with ReadP it's
-- not obvious.

module Main where

import System.IO (readFile')
import Data.List (foldl')

--  modules for parsing
import Data.Char (isDigit)
import Control.Monad (void)
import Control.Monad.Loops (whileM_)
import Control.Monad.Extra (andM)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,(<++)
  ,sepBy1
  ,munch1
  ,char
  ,string
  ,look
  ,get
  )

data Expr = Do | Dont | Val Int

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
-- the results given by readDatas since ReadP gives
-- us all results from smallest to biggest.
parseDatas :: String -> [Expr]
parseDatas str =
  case  readP_to_S readDatas str of
    xs@(_:_) -> fst (last xs)
    []  -> error "Error: parseDatas: can't parse."

readDatas :: ReadP [Expr]
readDatas = do
  whileNoExpr
  *> sepBy1 readExpr whileNoExpr

whileNoExpr :: ReadP ()
whileNoExpr = whileM_ noExpr get

noExpr :: ReadP Bool
noExpr =
  andM [noParse readMul_
       ,noParse (void (string "do()"))
       ,noParse (void (string "don't()"))
       ]

noParse :: ReadP () -> ReadP Bool
noParse p = do
  s <- look
  case readP_to_S p s of
    [] -> pure True
    _  -> pure False

-- Here, we can sequence_ over a list of parser,
-- but it is a bit slower.
readMul_ :: ReadP ()
readMul_ = do
  void (string "mul(")
  void (munch1 isDigit)
  void (char ',')
  void (munch1 isDigit)
  void (char ')')

readExpr :: ReadP Expr
readExpr = readDo <++ readDont <++ readMul

-- Hlint suggested to use ($>), so we tried.
readDont :: ReadP Expr
readDont = string "don't()" $> Dont

readDo :: ReadP Expr
readDo = string "do()" $> Do

readMul :: ReadP Expr
readMul = do
  void (string "mul(")
  n1 <- positive
  void (char ',')
  n2 <- positive
  void (char ')')
  pure (Val (n1 * n2))
