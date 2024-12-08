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

data Op = Add | Mul | Concat deriving (Show, Eq)

showSolution :: Show a => String -> a -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

main :: IO ()
main = do
  eqs <- parseDatas <$> readFile "day7.txt"
  showSolution "Part1" (partx 2 eqs)
  showSolution "Part2" (partx 3 eqs)

partx :: Int -> [Equation] -> Int
--partx p eqs = sum (map (checkEquation p) eqs)
partx p eqs = foldr f 0 eqs
  where
    f eq acc = acc + checkEquation p eq

-- returns 0 if there is no way to check the equation,
-- else returns the searched value
-- TODO: further simplified
checkEquation :: Int -> Equation -> Int
checkEquation p eq = if checks then val else 0
  where
    val = value eq
    lg = length (numbers eq) - 1
    opss = tails (cycle (deBruijnSequence lg p))
    opss' = take (p^lg) (map (take lg) opss)

    errorCheck = error "Error: checkEquation: the list of numbers is too short"
    (n, nums) = case numbers eq of
                  [] -> errorCheck
                  [_] -> errorCheck
                  (n':nums') -> (n', nums')

    checks = any helper opss'

    helper = (val ==) . fst . foldl' go (n, False) . zip nums

    go (acc, prune) (x, op)
      | prune     = (0, True)
      | otherwise = (acc', prune')
        where
          acc' = applyOp op acc x
          prune' = acc' > val

    applyOp Add a b = a+b
    applyOp Mul a b = a*b
    applyOp Concat a b = read (show a <> show b)

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


deBruijnSequence :: Int -> Int -> [Op]
deBruijnSequence n k =
    toOps .
    concat .
    filter ((==0) . mod n . length) .
    takeWhile (not . null) .
    iterate (nextLyndonWord n k) $ [0]
   where
     toOps = map ([Add, Mul, Concat] !!)
