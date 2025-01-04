-- AoC 2024 day 7
-- 18 times faster than the previous version.
-- We mixed the two version De Bruijn and Applicative.
-- We can choose the method passing an argument either DeBruijn
-- or anything else. Default to Applicative since it is faster.

{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

{- Results:
   Part1: 4122618559853
   Part2: 227615740238334
-}

module Main where

import System.IO (readFile')
import System.Environment (getArgs)
import Data.List (foldl', tails)
import Data.IntMap.Strict ((!))
import Data.IntMap.Strict qualified as IntMap

-- modules for parsing
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

-- | Equation represent a target @value@ and @numbers@
-- to reach this @value@ by combining @numbers@ with 2
-- or 3 operations (Add, Mul and Concat).
-- @count@ is equal to length of @numbers@ - 1, since
-- we need only @count@ operations to combine @count@+1
-- @numbers@
data Equation =
  Equation {value :: Int
           ,count :: Int
           ,numbers :: [Int]}
           deriving (Show)

data Op = Add | Mul | Concat
  deriving (Show, Eq)

operations :: [Op]
operations = [Add, Mul, Concat]

showSolution :: Show a => String -> a -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

data Method = APPLICATIVE | DEBRUIJN

parseMethod :: [String] -> Method
parseMethod ["DeBruijn"] = DEBRUIJN
parseMethod _            = APPLICATIVE

main :: IO ()
main = do
  method <- parseMethod <$> getArgs
  eqs <- parseDatas <$> readFile' "day7.txt"
  showSolution "Part1" (partx method 2 eqs)
  showSolution "Part2" (partx method 3 eqs)

-- | @partx method p eqs@ @method@ is either APPLICATIVE or DEBRUIJN.
-- @p@ is equal to 2 (for Part 1) or 3 (for Part 2).
-- For p == 2, we use only two operations Add and Mul.
-- For p == 3, we use Three oprations: Add, Mul and Concat.
-- @eqs@ is the list of Equation to try.
-- We Use an IntMap to memoize the list [[Op]] respective to the
-- value of (count eq) for each Equation. These values are between
-- 3 and 10.
partx :: Method -> Int -> [Equation] -> Int
partx method p eqs = fst (foldl' f (0, IntMap.empty) eqs)
  where
    permutations APPLICATIVE cnt = replicateM cnt (take p operations)
    permutations DEBRUIJN cnt = take (p^cnt) (map (take cnt) ops)
      where ops = tails (cycle (deBruijnSequence cnt p))

    f (acc, opss) eq = (acc', opss')
      where
        val = value eq
        n = count eq

        acc' = acc + checkEquation (opss' ! n) val eq

        opss' | n `IntMap.member` opss = opss
              | otherwise       = IntMap.insert n (permutations method n) opss

-- | @checkEquation opss val eq@ returns 0 if there is no
-- way to check the equation, else returns the searched value.
-- @eq@ is just the Equation to try
-- @opps@ are all permutations of Op with the length (count eq).
-- @val@ is (value eq), i.e. the searched value.
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
    -- Then the operations are applied in reverse order to the nums,
    -- and we start with the target val
    helper :: [Op] -> Bool
    helper = (== n)
             . fst
             . foldr go (val, False)
             . zip nums

    go (x, op) (acc, prune)
      | prune     = (0, True)
      | otherwise = applyOp op x acc

-- reverse operations. Thanks to glguy.
-- from: https://github.com/glguy/advent/blob/main/solutions/src/2024/07.hs
applyOp :: Op -> Int -> Int -> (Int, Bool)
applyOp Add a b
  | b > a     = (b - a, False)
  | otherwise = (0, True)
applyOp Mul a b
  | (q, r) <- b `quotRem` a
  ,r == 0    = (q, False)
  |otherwise = (0, True)
applyOp Concat a b = concatOp a b

concatOp :: Int -> Int -> (Int, Bool)
concatOp 0 b
  | b > 0     = (b, False)
  | otherwise = (0, True)
concatOp a b
  |(qa, ra) <- a `quotRem` 10
  ,(qb, rb) <- b `quotRem` 10
  , ra == rb = concatOp qa qb
concatOp _ _ = (0, True)

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
     toOps = map (operations !!)
