-- AoC 2024, day 10

{- HLINT ignore "Eta reduce" -}

import Data.Map.Strict qualified as M
import Data.Map.Strict (Map
                       ,(!)
                       )

import Data.Set qualified as S
import Data.Set (Set)
import Data.List (foldl')

-- module for parsing
import Data.Char (isDigit, digitToInt)
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
type Positions = Set (Int, Int)
type Trails = Map (Int, Int) Int

moves :: [(Int, Int)]
moves = [(0,1), (1, 0), (-1, 0), (0, -1)]

getDatas :: String -> IO Grid
getDatas filename = parseDatas <$> readFile filename

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  grid <- getDatas "day10.txt"
  let ths = trailHeads grid
  printSolution "Part1" (part1 grid ths)
  printSolution "Part2" (part2 grid ths)


part1 :: Grid -> [(Int, Int)] -> Int
part1 grid = foldl' f 0
  where
    f acc th = acc + S.size (score1 grid S.empty th)

score1 :: Grid -> Positions -> (Int, Int) -> Positions
score1 grid visited pos = foldl' f visited (nextPositions grid pos)
  where
    f vis p
      | (grid ! p) == 9 = S.insert p vis
      | otherwise       = score1 grid vis p


part2 :: Grid -> [(Int, Int)] -> Int
part2 grid = foldl' f 0
  where
    f acc th = acc + M.foldl' g 0 (countTrails grid M.empty th)
    g acc v = acc + v

countTrails :: Grid -> Trails -> (Int, Int) -> Trails
countTrails grid visited pos = foldl' f visited (nextPositions grid pos)
  where
    f vis p
      | grid ! p == 9 = M.alter g  p vis
      | otherwise   = countTrails grid vis p

    g Nothing  = Just 1
    g (Just x) = Just (x+1)

doMove :: (Int, Int) -> (Int, Int) -> (Int, Int)
doMove (x, y) (mx, my) = (x+mx, y+my)

nextPositions :: Grid -> (Int, Int) -> [(Int, Int)]
nextPositions grid pos = foldr f [] moves
  where
    val = grid ! pos
    f move acc
      | M.member pos' grid && val' - val == 1 = pos' : acc
      | otherwise                             = acc
      where
        pos' = doMove pos move
        val' = grid ! pos'

trailHeads :: Grid -> [(Int, Int)]
trailHeads grid = M.foldrWithKey' f [] grid
  where
    f pos v acc = if v == 0 then pos:acc else acc

parse :: ReadP a -> ReadS a
parse = readP_to_S

digit :: ReadP Int
digit = digitToInt <$> satisfy isDigit

parseDatas :: String -> Grid
parseDatas str = case parse readDatas str of
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
