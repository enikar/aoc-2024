-- AoC 2024 day 14, part1

{- HLINT ignore "Eta reduce" -}
module Main (main) where

import Data.Map.Strict
  (Map
  ,alter
  ,findWithDefault
  ,foldlWithKey'
  ,empty
  )
import Data.List (foldl')

-- module for parsing
import Data.Char (isDigit)
import Control.Monad (void)
import Text.ParserCombinators.ReadP
  (ReadP
  ,(<++)
  ,readP_to_S
  ,char
  ,string
  ,sepBy1
  ,munch1
  ,skipMany1
  ,optional
  ,option
  ,eof
  )

type Robot = (Int, Int)    -- a pair of velocities (vx, vy)
type Position = (Int, Int) -- columns and rows
type Grid = Map Position [Robot]

main :: IO ()
main = do
  grid <- getDatas "day14.txt"
  printSolution "Part1" (part1 grid)
  --printSolution "Part2" (part2 grid)

part1 :: Grid -> Int
part1 grid = product (map (countRobots grid') quadrants)
  where
    grid' = times 100 tick grid

-- TODO: part2
-- part2 :: Grid -> Int
-- part2 = undefined

countRobots :: Grid -> [Position] -> Int
countRobots grid coords = foldl' f 0 coords
  where
    f acc (x, y) = acc + length robots
      where robots = findWithDefault [] (x, y) grid

tickWith :: (Int -> Int -> Int -> Int -> (Int, Int)) -> Grid -> Grid
tickWith h = foldlWithKey' f initialGrid
  where
    f grid (px, py) robots = foldl' g grid robots
      where
        g acc r@(vx, vy) = alter (updatePosition r)
                                 (h px py vx vy)
                                 acc
tick :: Grid -> Grid
tick = tickWith h
  where
    h px py vx vy = (move columns px vx, move rows py vy)

updatePosition :: Robot -> Maybe [Robot] -> Maybe [Robot]
updatePosition robot Nothing = Just [robot]
updatePosition robot (Just robots) = Just (robot:robots)

move :: Int -> Int -> Int -> Int
move modulo pos vel
  | pos' < 0  = pos' + modulo
  | pos' < modulo = pos'
  | otherwise = pos' - modulo
    where pos' = pos + vel

-- borrow from: https://github.com/glguy/advent/blob/main/common/src/Advent/Prelude.hs
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x


-- parsing stuff
number :: ReadP Int
number = do
  signed <- option '+' (char '-' <++ char '+')
  n <- read <$> munch1 isDigit
  let n' | signed == '+' = n
         | otherwise     = -n
  pure n'

newLine :: ReadP ()
newLine = void (char '\n')

spaces :: ReadP ()
spaces = skipMany1 (char ' ')

parseDatas :: String -> Grid
parseDatas str =
  case readP_to_S readDatas str of
    [(grid, "")] -> grid
    []  -> error "Error: parseDatas: can't parse."
    _   -> error "Error: parseDatas: there are more than one result."

readDatas :: ReadP Grid
readDatas = do
  robots <- sepBy1 readRobot newLine
  optional newLine
  eof
  pure (createGrid robots)

readRobot :: ReadP (Position, Robot)
readRobot = do
  void (string "p=")
  px <- number
  void (char ',')
  py <- number
  spaces
  void (string "v=")
  vx <- number
  void (char ',')
  vy <- number
  pure ((px, py), (vx, vy))

createGrid :: [(Position, Robot)] -> Grid
createGrid = foldl' f initialGrid
  where
    f acc (p, r) = alter (updatePosition r) p acc

-- tools
columns, rows :: Int
columns = 101
rows = 103
xhalf, yhalf :: Int
xhalf = columns `quot` 2
yhalf = rows `quot` 2

initialGrid :: Grid
initialGrid = empty

quadrantLT, quadrantLB, quadrantRT, quadrantRB :: [Position]
quadrantLT = [(x, y)
              |x <- [0 .. xhalf-1]
              , y <- [0 .. yhalf-1]
              ]
quadrantLB = [(x,y)
              |x <- [0 .. xhalf-1]
              ,y <- [yhalf+1 .. rows-1]
              ]
quadrantRT = [(x,y)
              |x <- [xhalf+1 .. columns-1]
              ,y <- [0 .. yhalf-1]
              ]
quadrantRB = [(x, y)
              |x <- [xhalf+1 .. columns-1]
              ,y <- [yhalf+1 .. rows-1]
              ]

quadrants :: [[Position]]
quadrants = [quadrantLT, quadrantRT, quadrantLB, quadrantRB]

getDatas :: String -> IO Grid
getDatas filename = parseDatas <$> readFile filename

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)
