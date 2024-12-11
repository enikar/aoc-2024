-- AoC 2024, day 10 "optimized" version.

{- HLINT ignore "Eta reduce" -}

import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Map.Strict
  (Map
  ,(!)
  )

-- module for parsing
import Data.Char
  (isDigit
  ,digitToInt
  )
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,char
  ,sepBy1
  ,many1
  ,optional
  ,eof
  ,satisfy
  )

type Grid = Map (Int,Int) Int
type Trails = Map (Int,Int) Int

moves :: [(Int, Int)]
moves = [(0,1), (1,0), (-1,0), (0,-1)]

getDatas :: String -> IO Grid
getDatas filename = parseDatas <$> readFile filename

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)


-- we precompute an intermediate result for solving part1 and
-- part2. In this way all Trails are traversed only once.
main :: IO ()
main = do
  grid <- getDatas "day10.txt"
  let trails = allTrails grid
  printSolution "Part1" (partx M.size trails)
  printSolution "Part2" (partx sumTrails trails)


allTrails :: Grid -> [Trails]
allTrails grid = foldl' f [] (trailHeads grid)
  where
    f acc th = trails : acc
      where
        trails = countTrails grid M.empty th

partx :: (Trails -> Int) ->  [Trails] -> Int
partx f = foldl' g 0
  where
    g acc trails = acc + f trails

sumTrails :: Trails -> Int
sumTrails = M.foldl' (+) 0

countTrails :: Grid -> Trails -> (Int, Int) -> Trails
countTrails grid visited pos = foldl' f visited (nextPositions grid pos)
  where
    f vis p
      | grid ! p == 9 = M.alter g  p vis
      | otherwise     = countTrails grid vis p

    g Nothing  = Just 1
    g (Just x) = Just (x+1)

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (mx, my) = (x+mx, y+my)

nextPositions :: Grid -> (Int, Int) -> [(Int, Int)]
nextPositions grid pos = foldl' f [] moves
  where
    val = grid ! pos
    f acc mv
      | M.member pos' grid && val' - val == 1 = pos' : acc
      | otherwise                             = acc
      where
        pos' = move pos mv
        val' = grid ! pos'

trailHeads :: Grid -> [(Int, Int)]
trailHeads grid = M.foldrWithKey' f [] grid
  where
    f pos v acc = if v == 0 then pos:acc else acc

-- parsing stuff
parse :: ReadP a -> ReadS a
parse = readP_to_S

digit :: ReadP Int
digit = digitToInt <$> satisfy isDigit

parseDatas :: String -> Grid
parseDatas str =
  case parse readDatas str of
    [x] -> fst x
    []  -> error "Error: parseDatas: can't parse."
    _   -> error "Error: parseDatas: there are more than one result."

readDatas :: ReadP Grid
readDatas = do
  grid <- buildGrid <$> sepBy1 readRow (char '\n')
  optional (char '\n')
  eof
  pure grid

readRow :: ReadP [Int]
readRow = many1 digit

buildGrid :: [[Int]] -> Grid
buildGrid lss = foldl' f M.empty (zip [0..] lss)
  where
    f acc (y, ls) = buildRow acc  y ls

buildRow :: Grid -> Int -> [Int] -> Grid
buildRow grid row ls = foldl' f grid (zip [0..] ls)
   where
     f acc (x, n) = M.insert (x, row) n acc
