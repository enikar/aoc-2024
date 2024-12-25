-- AoC 2024, day 6, part1
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}
module Main(main) where

import System.IO (readFile')
import Data.Array.Unboxed
  (UArray
  ,array
  ,assocs
  ,bounds
  ,range
  ,(!)
  )

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Foldable (for_)
import Data.List (find)

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

directions :: [Char]
directions = "^v><"

main :: IO ()
main = do
  grid <- getDatas "day6.txt"
  let path0 = initialPath grid
  printSolution "Part1" (part1 path0)
  --printSolution "Part2" (part2 path0 grid)


part1 :: Visited -> Int
part1 visited = Set.size (visitedToPositions visited)

visitedToPositions :: Visited -> Set Position
visitedToPositions visited = Set.foldl' f Set.empty visited
  where
    f acc g = case position g of
      Just p -> Set.insert p acc
      Nothing -> acc

initialPath :: Grid -> Visited
initialPath grid0 = go grid0 Set.empty
  where
    go (_, Exit) visited = visited
    go grid visited      =  go grid' visited'
      where
        (grid', visited') = next (grid, visited)

position :: Guardian -> Maybe Position
position (Up p)     = Just p
position (Down p)   = Just p
position (DRight p) = Just p
position (DLeft p)  = Just p
position _          = Nothing

next :: (Grid, Visited) -> (Grid, Visited)
next (grid, visited) = (grid', visited')
  where
    (arr, _) = grid
    grid' = (arr, g')
    visited' = Set.union visited visited1

    (visited1, g') = move grid

findObstacle :: [(Int, Char)] -> Maybe (Int, Char)
findObstacle = find (('#' ==) . snd)

move :: Grid -> (Visited, Guardian)
move (arr, g) = case g of
  Up p -> moveUp arr p
  Down p -> moveDown arr p
  DRight p -> moveRight arr p
  DLeft p -> moveLeft arr p
  _       -> noMove arr (-1, -1)


noMove, moveUp, moveDown, moveRight, moveLeft :: Move
noMove _ _ = error "Error: noMove is called!"

moveUp arr (x0, y0) =
  case findObstacle column of
     Nothing     -> (visited 0, Exit)
     Just (y, _) -> let y' = y+1
                    in (visited y', DRight (x0, y'))
  where
    column   = reverse [(y, c) | ((x, y), c) <- assocs arr, x == x0, y < y0 ]
    visited n = Set.fromList [Up (x0, y) |y <- [n..y0]]

moveDown arr (x0, y0) =
  case findObstacle column of
    Nothing     -> (visited ysup, Exit)
    Just (y, _) -> let y' = y - 1
                   in (visited y', DLeft (x0, y-1))
  where
    column = [(y, c) | ((x, y), c) <- assocs arr, x == x0, y > y0]
    visited n = Set.fromList [Down (x0, y) |y <- [y0..n]]
    (_, (_, ysup)) = bounds arr

moveRight arr (x0, y0) =
  case findObstacle row of
    Nothing     -> (visited xsup, Exit)
    Just (x, _) -> let x' = x - 1
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

getDatas :: String -> IO Grid
getDatas filename = parseDatas <$> readFile' filename

guardian :: Position -> Char -> Maybe Guardian
guardian p c = case c of
  '^' -> Just (Up p)
  'v' -> Just (Down p)
  '>' -> Just (DRight p)
  '<' -> Just (DLeft p)
  _   -> Nothing

-- Using two fold to build the Array and to find the
-- guardian in a single path is slower than first build
-- the Array, then find the guardian.
parseDatas :: String -> Grid
parseDatas s = (arr, g)
  where
    arr = buildGrid s
    g = case findGuardian arr of
          Just g' -> g'
          Nothing -> error "Error: parseDatas can't find the guardian."

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

-- tools for ghci
showGridVisited :: Grid -> Set Guardian -> [String]
showGridVisited grid visited = foldr f [] (range (yinf, ysup))
  where
    gpos = visitedToPositions visited
    (arr, g) = grid
    ((xinf, yinf), (xsup, ysup)) = bounds arr
    (cg, (x0, y0)) = case g of
      Up p     -> ('^', p)
      Down p   -> ('v', p)
      DRight p -> ('>', p)
      DLeft p  -> ('<', p)
      Exit     -> ('Q', (-1, -1))
      Loop     -> ('O', (-1, -1))

    f y strs = foldr h [] (range (xinf, xsup)) : strs
      where
        h x str = c : str
          where
            c0  = arr ! (x, y)
            c | (x, y) == (x0, y0)      = cg
              | (x,y) `Set.member` gpos = 'X'
              | c0 `elem` directions    = '.'
              | otherwise               = c0

showGrid :: Grid -> [String]
showGrid grid = showGridVisited grid Set.empty

printGrid :: Grid -> IO ()
printGrid grid = printGridVisited grid Set.empty

printGridVisited :: Grid -> Set Guardian -> IO ()
printGridVisited grid visited = for_ (showGridVisited grid visited) putStrLn
