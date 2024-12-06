-- AoC 2024, day 5

{- HLINT ignore "Eta reduce" -}

module Main where

import Data.Char (isDigit)
import Control.Monad (void)
import Data.IntMap.Strict qualified as M
import Data.IntMap.Strict (IntMap)
import Data.List (sortBy, partition, foldl')
import Data.IntSet qualified as S
import Data.IntSet (IntSet)

import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,char
  ,sepBy1
  ,munch1
  ,optional
  ,eof
  )

type Datas = (IntMap IntSet, [[Int]])

showSolution :: String -> Int -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

main :: IO ()
main = do
  (rules, updates) <- parseDatas <$> readFile "day5.txt"
  let (ordered, nonOrdered) = partition (sorted rules) updates
  showSolution "Part1" (part1 ordered)
  showSolution "Part2" (part2 rules nonOrdered)

part1 :: [[Int]] -> Int
part1 updates = sum (map middle updates)

sorted :: IntMap IntSet -> [Int] -> Bool
sorted _ [] = True
sorted rules (x:xs) = all p xs && sorted rules xs
  where
    p y = let o = simpleCmp rules x y
          in o == LT || o == EQ

part2 :: IntMap IntSet -> [[Int]] -> Int
part2 rules updates = sum (map (middle . sortByRules) updates)
  where
    sortByRules = sortBy (simpleCmp rules)

simpleCmp :: IntMap IntSet -> Int -> Int -> Ordering
simpleCmp rules x y = choice xy yx
  where
    xy = (y `S.member`) <$> M.lookup x rules
    yx = (x `S.member`) <$> M.lookup y rules

    choice (Just True) _          = LT
    choice _          (Just True) = GT
    choice _           _          = EQ

middle :: [Int] -> Int
middle ls = ls !! n
  where n = length ls `quot` 2

parse :: ReadP a -> ReadS a
parse = readP_to_S

number :: ReadP Int
number = read <$> munch1 isDigit

parseDatas :: String -> Datas
parseDatas str = (fst . head) (parse readDatas str)

readDatas :: ReadP Datas
readDatas = do
  rules <- buildRules <$> sepBy1 readRule (char '\n')
  void (char '\n')
  void (char '\n')
  updates <- sepBy1 readUpdate (char '\n')
  optional (char '\n')
  eof
  pure (rules, updates)


readRule :: ReadP (Int, Int)
readRule = do
  n1 <- number
  void (char '|')
  n2 <- number
  pure (n1, n2)

buildRules :: [(Int, Int)] -> IntMap IntSet
buildRules xs = foldl' f M.empty xs
  where
    f acc (n1, n2) =
      case M.lookup n1 acc of
           Nothing -> M.insert n1 (S.singleton n2) acc
           Just set -> M.insert n1 (S.insert n2 set) acc

readUpdate :: ReadP [Int]
readUpdate = sepBy1 number (char ',')
