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
import Data.List
  (foldl'
  ,find
  )
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

type Position = (Int, Int)
data Direction = Up
               | Down
               | DRight
               | DLeft
               | Exit
               deriving (Show, Eq)

type Guardian = (Position, Direction)
type Grid = (UArray Position Char, Guardian)
type Visited = Set Position
type Move = UArray Position Char -> Position -> (Visited, Maybe Position)

directions :: [(Direction, Char)]
directions = [(Up, '^')
             ,(Down, 'v')
             ,(DRight, '>')
             ,(DLeft, '<')
             ,(Exit, 'O')
             ]

moves :: [(Direction, Move)]
moves = [(Up, moveUp)
        ,(Down, moveDown)
        ,(DRight, moveRight)
        ,(DLeft, moveLeft)
        ,(Exit, noMove)
        ]

main :: IO ()
main = do
  grid <- getDatas "day6.txt"
  printSolution "Part1" (part1 grid)
  --printSolution "Part2" (part2 grid)


part1 :: Grid -> Int
part1 grid = Set.size visited
  where
    visited = fst (until satisfy next (Set.empty, grid))
      where
       satisfy (_, (_, (_, m))) = m == Exit

next :: (Visited, Grid) -> (Visited, Grid)
next (visited0, grid) = (visited, grid')
  where
    (arr, (p0, dir)) = grid
    grid' = (arr, (p', dir'))
    visited = Set.union visited0 visited1
    p' = fromMaybe (-1,-1) p
    dir' |p' == (-1, -1) = Exit
         |otherwise      = turnRight dir

    (visited1, p) = move arr p0
    move = fromMaybe errorNext (lookup dir moves)
    errorNext = error "Error: next: impossible direction."

findObstacle :: [(Int, Char)] -> Maybe (Int, Char)
findObstacle = find (('#' ==) . snd)

noMove, moveUp, moveDown, moveRight, moveLeft :: Move
noMove _ _ = (Set.empty, Nothing)

moveUp arr (x0, y0) =
  case findObstacle column of
     Nothing     -> (visited 0, Nothing)
     Just (y, _) -> let y' = y+1
                    in (visited y', Just (x0, y'))
  where
    column   = reverse [(y, c) | ((x, y), c) <- assocs arr, x == x0, y < y0 ]
    visited n = Set.fromList [(x0, y) |y <- [n..y0]]

moveDown arr (x0, y0) =
  case findObstacle column of
    Nothing     -> (visited ysup, Nothing)
    Just (y, _) -> let y' = y - 1
                   in (visited y', Just (x0, y-1))
  where
    column = [(y, c) | ((x, y), c) <- assocs arr, x == x0, y > y0]
    visited n = Set.fromList [(x0, y) |y <- [y0..n]]
    (_, (_, ysup)) = bounds arr

moveRight arr (x0, y0) =
  case findObstacle row of
    Nothing     -> (visited xsup, Nothing)
    Just (x, _) -> let x' = x - 1
                   in (visited x', Just (x',y0))
  where
    row = [(x, c) | ((x,y), c) <- assocs arr, y == y0, x > x0]
    visited n = Set.fromList [(x, y0) | x <- [x0..n]]
    (_, (xsup, _)) = bounds arr

moveLeft arr (x0, y0) =
  case findObstacle row of
    Nothing     -> (visited 0, Nothing)
    Just (x, _) -> let x' = x+1
                   in (visited x', Just (x', y0))
  where
    row = reverse [(x, c) | ((x,y),c) <- assocs arr, y == y0, x < x0]
    visited n = Set.fromList [(x, y0) | x <- [n..x0]]

turnRight :: Direction -> Direction
turnRight Up = DRight
turnRight Down = DLeft
turnRight DRight = Down
turnRight DLeft = Up
turnRight Exit = Exit

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

getDatas :: String -> IO Grid
getDatas filename = parseDatas <$> readFile' filename

parseDatas :: String -> Grid
parseDatas s = (arr, guardian)
  where
    arr = buildGrid (lines s)
    guardian = fromMaybe parseError (foldl' f Nothing (assocs arr))
    parseError = error "Error: parseDatas can't find the guardian."

    dirs = map swap directions

    f p@(Just _) _ = p
    f Nothing ((x,y), c) = lookup c dirs >>= g
      where
        g dir = Just ((x,y), dir)

buildGrid :: [String] -> UArray Position Char
buildGrid ss = array ((0, 0),(width, height)) cs
  where
    width = case ss of
              [] -> 0
              (s:_) -> length s - 1
    height = length ss -1
    cs = [((x,y), c)
         |(y, s) <- zip [0..] ss
         ,(x, c) <- zip [0..] s
         ]

-- tools for ghci
showGrid :: Grid -> [String]
showGrid grid = foldr f [] (range (yinf, ysup))
  where
    (arr, ((x0, y0), dir)) = grid
    ((xinf, yinf), (xsup, ysup)) = bounds arr
    dirs = map snd directions

    f y strs = foldr g [] (range (xinf, xsup)) : strs
      where
        g x str = c : str
          where
            c0  = arr ! (x, y)
            c | (x, y) == (x0, y0) = fromMaybe '.' (lookup dir directions)
              | c0 `elem` dirs     = '.'
              | otherwise          = c0

printGrid :: Grid -> IO ()
printGrid grid = for_ (showGrid grid) putStrLn
