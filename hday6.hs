-- AoC 2024, day 6
-- Naive solution. It is quite slow.

-- TODO: rewrite it using Data.Map.Strict instead of UArray

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

{- HLINT ignore "Eta reduce" -}
module Main(main) where

import System.IO (readFile')
import Data.Array.Unboxed
  (UArray
  ,array
  ,assocs
  ,bounds
  ,(//)
  )

import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (find)
import Data.Maybe (fromMaybe)

type Position = (Int, Int)
data Guardian = Up Position
               | Down Position
               | DRight Position
               | DLeft Position
               | Exit
               | Loop
               deriving (Show, Eq, Ord)

type Grid = (UArray Position Char, Guardian)
type Visited = Set Guardian
type Move = UArray Position Char -> Position -> (Visited, Guardian)

main :: IO ()
main = do
  grid <- getDatas "day6.txt"
  let visited = initialPath grid
  printSolution "Part1" (part1 visited)
  printSolution "Part2" (part2 visited grid)

part1 :: Visited -> Int
part1 visited = Set.size (visitedToPositions visited)

-- Here we write a kind of (Set.mapMaybe position)
visitedToPositions :: Visited -> Set Position
visitedToPositions visited = Set.foldr f Set.empty visited
    where
      f x s = maybe s (`Set.insert` s) (position x)

position :: Guardian -> Maybe Position
position (Up p)     = Just p
position (Down p)   = Just p
position (DRight p) = Just p
position (DLeft p)  = Just p
position _          = Nothing

initialPath :: Grid -> Visited
initialPath grid0 = go grid0 Set.empty
  where
    go (_, Exit) visited = visited
    go grid visited      = uncurry go (next grid visited)

part2 :: Visited -> Grid -> Int
part2 visited grid = foldl' f 0 positions
  where
    positions = Set.toList (visitedToPositions visited)
    (arr0, g0) = grid
    p0 = fromMaybe errorPart2 (position g0)
    errorPart2 = error "Error: part2: can't find the start"

    f acc p
      | p == p0   = acc
      | g == Exit = acc
      | g == Loop = acc+1
      | otherwise = acc -- not reach
      where
        arr1 = arr0 // [(p, '#')]
        g = untilEnd (arr1, g0)

untilEnd :: Grid -> Guardian
untilEnd grid0 = go grid0 Set.empty
  where
    go (_, Exit) _ = Exit
    go grid visited
      | g `Set.member` visited = Loop
      | otherwise              = go grid' visited'
      where
        (grid'@(_, g), visited') = next grid visited

next :: Grid -> Visited -> (Grid, Visited)
next grid visited = (grid', visited')
  where
    (arr, _) = grid
    grid' = (arr, g')
    visited' = Set.union visited visited1
    (visited1, g') = move grid

findObstacle :: [(Int, Char)] -> Maybe (Int, Char)
findObstacle = find (('#' ==) . snd)
{-# INLINE findObstacle #-}

move :: Grid -> (Visited, Guardian)
move (arr, g) = case g of
  Up p     -> moveUp arr p
  Down p   -> moveDown arr p
  DRight p -> moveRight arr p
  DLeft p  -> moveLeft arr p
  _        -> noMove arr (-1, -1)

noMove, moveUp, moveDown, moveRight, moveLeft :: Move
noMove _ _ = error "Error: noMove is called!"

moveUp arr (x0, y0) =
  case findObstacle column of
     Nothing     -> (visited 0, Exit)
     Just (y, _) -> let y' = y+1
                    in (visited y', DRight (x0, y'))
  where
    column   = reverse [(y, c) | ((x, y), c) <- assocs arr, x == x0, y < y0]
    visited n = Set.fromList [Up (x0, y) |y <- [n..y0]]

moveDown arr (x0, y0) =
  case findObstacle column of
    Nothing     -> (visited ysup, Exit)
    Just (y, _) -> let y' = y-1
                   in (visited y', DLeft (x0, y'))
  where
    column = [(y, c) | ((x, y), c) <- assocs arr, x == x0, y > y0]
    visited n = Set.fromList [Down (x0, y) |y <- [y0..n]]
    (_, (_, ysup)) = bounds arr

moveRight arr (x0, y0) =
  case findObstacle row of
    Nothing     -> (visited xsup, Exit)
    Just (x, _) -> let x' = x-1
                   in (visited x', Down (x',y0))
  where
    row = [(x, c) | ((x,y), c) <- assocs arr, y == y0, x > x0]
    visited n = Set.fromList [DRight (x, y0) | x <- [x0..n]]
    (_, (xsup, _)) = bounds arr

moveLeft arr (x0, y0) =
  case findObstacle row of
    Nothing     -> (visited 0, Exit)
    Just (x, _) -> let x' = x+1
                   in (visited x', Up (x', y0))
  where
    row = reverse [(x, c) | ((x,y),c) <- assocs arr, y == y0, x < x0]
    visited n = Set.fromList [DLeft (x, y0) | x <- [n..x0]]


printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

-- parsing and intializations
getDatas :: String -> IO Grid
getDatas filename = parseDatas <$> readFile' filename

-- Using two fold to build the Array and to find the
-- guardian in a single path is slower than first build
-- the Array, then find the guardian.
parseDatas :: String -> Grid
parseDatas s = (arr, g)
  where
    arr = buildGrid s
    g = case findGuardian arr of
          Just g' -> g'
          Nothing -> error "Error: parseDatas can't find the guardian"

directions :: [Char]
directions = "^v><"

findGuardian :: UArray Position Char -> Maybe Guardian
findGuardian arr = uncurry guardian =<<
                   find ((`elem` directions) . snd)
                        (assocs arr)

buildGrid :: String -> UArray Position Char
buildGrid str = array ((0, 0),(width, height)) cs
  where
    ss = lines str
    width = case ss of
              [] -> 0
              (s:_) -> length s - 1
    height = length ss - 1
    cs = [((x,y), c)
         |(y, s) <- zip [0..] ss
         ,(x, c) <- zip [0..] s
         ]

guardian :: Position -> Char -> Maybe Guardian
guardian p c = case c of
  '^' -> Just (Up p)
  'v' -> Just (Down p)
  '>' -> Just (DRight p)
  '<' -> Just (DLeft p)
  _   -> Nothing
