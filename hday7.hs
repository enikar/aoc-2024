-- AoC 2024 day 7
-- 12 times faster than the previous version.
-- We mixed the two version De Bruijn and Applicative.
-- We can choose the method passing an argument either DeBruijn
-- or anything else. Default to Applicative since it is faster.

{- HLINT ignore "Eta reduce" -}

{- Results:
   Part1: 4122618559853
   Part2: 227615740238334
-}

module Main where

import System.IO (readFile')
import System.Environment (getArgs)
import Data.List (foldl', tails)
import Data.Char (isDigit)
import Control.Monad (replicateM, void)
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

data Equation =
  Equation {value :: Int
           ,count :: Int
           ,numbers :: [Int]}
           deriving (Show)

data Op = Add | Mul | Concat
  deriving (Show, Eq)

data Method = APPLICATIVE | DEBRUIJN

showSolution :: Show a => String -> a -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

parseMethod :: [String] -> Method
parseMethod ["DeBruijn"] = DEBRUIJN
parseMethod _            = APPLICATIVE

main :: IO ()
main = do
  method <- parseMethod <$> getArgs
  eqs <- parseDatas <$> readFile' "day7.txt"
  showSolution "Part1" (partx method 2 eqs)
  showSolution "Part2" (partx method 3 eqs)

partx :: Method -> Int -> [Equation] -> Int
partx method p eqs = foldl' f 0 eqs
  where
    f acc eq = acc + checkEquation (opss method (count eq)) val eq
      where
        val = value eq

    opss APPLICATIVE cnt = replicateM cnt (take p ops)
      where ops = [Add, Mul, Concat]

    opss DEBRUIJN cnt = take (p^cnt) (map (take cnt) ops)
      where
        ops = tails (cycle (deBruijnSequence cnt p))

-- returns 0 if there is no way to check the equation,
-- else returns the searched value.
-- eq is just the Equation to try
-- opps are all permutations of Op with the length (count eq).
-- val is (value eq)
checkEquation :: [[Op]] -> Int -> Equation -> Int
checkEquation opss val eq = if checks then val else 0
  where
    errorCheck = error "Error: checkEquation: the list of numbers is too short"
    (n, nums) = case numbers eq of
                  [] -> errorCheck
                  [_] -> errorCheck
                  (n':nums') -> (n', nums')

    checks = any helper opss
    -- we use foldr to short-cicuit when possible.
    -- Then the operators are applied in reverse order to the num.
    helper :: [Op] -> Bool
    helper = (== n)
             . fst
             . foldr go (val, False)
             . zip nums

    go (x, op) (acc, prune)
      | prune     = (0, True)
      | otherwise = case applyOp op x acc of
          []  -> (0, True)
          [v] -> (v, False)
          _   -> error "Error: checkEquation!" -- not reach

-- reverse operators. Thanks to glguy.
-- from: https://github.com/glguy/advent/blob/main/solutions/src/2024/07.hs
applyOp :: Op -> Int -> Int -> [Int]
applyOp Add a b = [b - a | b > a ]
applyOp Mul a b = [q | let (q,r) = b `quotRem` a, r == 0]
applyOp Concat a b = concatOp a b

concatOp :: Int -> Int -> [Int]
concatOp 0 b = [b | b>0 ]
concatOp a b
  |(qa, ra) <- a `quotRem` 10
  ,(qb, rb) <- b `quotRem` 10
  , ra == rb = concatOp qa qb
concatOp _ _ = []

-- Parsing stuff
number :: ReadP Int
number = read <$> munch1 isDigit

spaces :: ReadP String
spaces = many1 (char ' ')

parseDatas :: String -> [Equation]
parseDatas str =
  case  readP_to_S readDatas str of
    [x] -> fst x
    []  -> error "Error: parseDatas: can't parse."
    _   -> error "Error: parseDatas: there are more than one result."

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
  pure (Equation n (length ls - 1) ls)

-- De Bruijn sequence
nextLyndonWord :: Int -> Int -> [Int] -> [Int]
nextLyndonWord n k = foldr checkLyndonElement [] . take n . cycle
    where
      checkLyndonElement :: Int -> [Int] -> [Int]
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
