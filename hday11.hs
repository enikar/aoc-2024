-- AoC 2024, Day 11, part1

{- HLINT ignore "Eta reduce" -}

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Bits (shiftR)


readInt :: String -> Integer
readInt s = fromMaybe errRead  (readMaybe s)
  where
    errRead = error ("Error: readInt: not an Int: " <> s)

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  stones <- map readInt . words <$> readFile "day11.txt"
  printSolution "Part1" (part1 25 stones)

part1 :: Int -> [Integer] -> Int
part1 m stones = (length . snd) (until satisfy improve (0::Int, stones))
  where
    satisfy (n, _) = n == m
    improve (n, stones') = (n+1, stones' >>= applyRules)

applyRules :: Integer -> [Integer]
applyRules n
  | n == 0      = [1]
  | n1 == 0     = [n * 2024]
  | otherwise   = [n1, n2]
     where (n1, n2) = splitIfEvenDigits n

splitIfEvenDigits :: Integer -> (Integer, Integer)
splitIfEvenDigits n
  | even dn    = (n1, n2)
  | otherwise  = (0, n)
  where
    dn = digitNumber n
    half = shiftR dn 1 -- divide by 2
    (n1, n2) = n `quotRem` (10^half)

digitNumber :: Integer -> Integer
digitNumber n = snd (until satisfy  improve (n, 0))
  where
    satisfy (n', _) = n' == 0
    improve (n', k) = (n' `quot` 10, k+1)
