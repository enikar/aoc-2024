-- AoC 2024 day 7

{- HLINT ignore "Eta reduce" -}

module Main where

import Data.Char (isDigit)
import Control.Monad (void)
import Data.List (tails, foldl')
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,char
  ,string
  ,sepBy1
  ,munch1
  ,many1
  ,optional
  ,eof
  )

data Equation = Equation {value :: Int, numbers :: [Int]}
           deriving (Show)

data Op = Add | Mul deriving (Show, Eq)

showSolution :: Show a => String -> a -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

main :: IO ()
main = do
  eqs <- parseDatas <$> readFile "day7.txt"
  showSolution "Part1" (part1 eqs)
  --showSolution "Part2" (part2 eqs)

part1 :: [Equation] -> Int
part1 eqs = sum (map checkEquation eqs)

-- returns 0 if there is no way to check the equation,
-- else returns the searched value
checkEquation :: Equation -> Int
checkEquation eq = if null checks then 0 else val
  where
    val = value eq
    n = length (numbers eq) - 1
    ops = tails (cycle (deBruijnSequence n))
    opss = take (2^n) (map (take n) ops)
    checks = filter (checkEquation' eq) opss

checkEquation' :: Equation -> [Op] -> Bool
checkEquation' eq ops = val == foldl' f n (zip nums ops)
  where
    val = value eq
    errorCheck = error "Error: checkEquation: the list of numbers is too short"
    (n, nums) = case numbers eq of
                  [] -> errorCheck
                  [_] -> errorCheck
                  (n':nums') -> (n', nums')

    f acc (x, Add) = acc + x
    f acc (x, Mul) = acc * x

-- TODO
-- part2

parse :: ReadP a -> ReadS a
parse = readP_to_S

number :: ReadP Int
number = read <$> munch1 isDigit

spaces :: ReadP String
spaces = many1 (char ' ')

parseDatas :: String -> [Equation]
parseDatas str = (fst . head) (parse readDatas str)

readDatas :: ReadP [Equation]
readDatas = do
  ds <- sepBy1 readData (char '\n')
  optional (char '\n')
  eof
  pure ds

readData :: ReadP Equation
readData = do
  n <- number
  void (string ": ")
  ls <- sepBy1 number spaces
  pure (Equation n ls)


nextLyndonWord :: Int -> Int -> [Int] -> [Int]
nextLyndonWord n k = foldr checkLyndonElement [] . take n . cycle
    where
    -- checkLyndonElement :: Int -> [Int] -> [Int]
       checkLyndonElement x [] = [x + 1 | x < k - 1]
       checkLyndonElement x xs = x:xs


deBruijnSequence :: Int -> [Op]
deBruijnSequence n  =
    toOps .
    concat .
    filter ((==0) . mod n . length) .
    takeWhile (not . null) .
    iterate (nextLyndonWord n 2) $ [0]
   where
     toOps = map ([Add, Mul] !!)
