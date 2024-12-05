-- AoC 2024, day 5

{- HLINT ignore "Eta reduce" -}

module Main where

import Data.Char (isDigit)
import Control.Monad (void)
import Data.IntMap.Strict qualified as M
import Data.IntMap.Strict (IntMap)
import Data.List (sortBy, partition)

import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,char
  ,sepBy1
  ,munch1
  ,optional
  ,eof
  )

data Datas = D { rules :: IntMap [Int], updates :: [[Int]]}
             deriving (Show)

showSolution :: String -> Int -> IO ()
showSolution part answer =
  putStrLn (part <> ": " <> show answer)

main :: IO ()
main = do
  datas <- parseDatas <$> readFile "day5.txt"
  let rls = rules datas
      ups = updates datas
      (ordered, nonOrdered) = partition (sorted rls) ups
  showSolution "Part1" (part1 ordered)
  showSolution "Part2" (part2 rls nonOrdered)

part1 :: [[Int]] -> Int
part1 ups = sum (map middle  ups)

sorted :: IntMap [Int] -> [Int] -> Bool
sorted _ [] = True
sorted rls (x:xs) = all p xs && sorted rls xs
  where
    p y = let o = simpleCmp rls x y
          in o == LT || o == EQ

part2 :: IntMap [Int] -> [[Int]] -> Int
part2 rls ups = sum (map (middle . sortByRules) ups)
  where
    sortByRules = sortBy (simpleCmp rls)

simpleCmp :: IntMap [Int] -> Int -> Int -> Ordering
simpleCmp rls x y =
  let xy = (y `elem`) <$> M.lookup x rls
      yx = (x `elem`) <$> M.lookup y rls
  in
    case xy of
      Just True -> LT
      _         -> case yx of
                     Just True -> GT
                     _         -> EQ

middle :: [Int] -> Int
middle ls = ls !! n
  where n = length ls `quot` 2

parse :: ReadP a -> ReadS a
parse = readP_to_S

number :: ReadP Int
number = read <$> munch1 isDigit

parseDatas :: String -> Datas
parseDatas s = (fst . head) (parse readDatas s)

readDatas :: ReadP Datas
readDatas = do
  rs <- buildMap <$> sepBy1 readRule (char '\n')
  void (char '\n')
  void (char '\n')
  us <- sepBy1 readUpdate (char '\n')
  optional (char '\n')
  eof
  pure (D rs us)


readRule :: ReadP (Int, Int)
readRule = do
  n1 <- number
  void (char '|')
  n2 <- number
  pure (n1, n2)

buildMap :: [(Int, Int)] -> IntMap [Int]
buildMap xs = foldr f M.empty xs
  where
    f (n1, n2) acc =
      case M.lookup n1 acc of
           Nothing -> M.insert n1 [n2] acc
           Just ls -> M.insert n1 (n2:ls) acc

readUpdate :: ReadP [Int]
readUpdate = sepBy1 number (char ',')
