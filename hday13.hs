-- AoC 2024, day 13

{- HLINT ignore "Eta Reduce" -}

import Data.List (foldl')
import Data.Ratio
  ((%)
  ,numerator
  ,denominator
  )
import Data.Maybe
  (isNothing
  ,fromMaybe
  )

-- module for parsing
import Data.Char
  (isDigit
  --,digitToInt
  )

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
  ,eof
  )

data Game = Game {aBut :: (Int, Int)
                 ,bBut :: (Int, Int)
                 ,prize :: (Int, Int)
                 } deriving (Show)

getDatas :: String -> IO [Game]
getDatas fileName = parseDatas <$> readFile fileName

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  games <- getDatas "day13.txt"
  printSolution "Part1" (partx games)
  printSolution "Part2" (partx (correctsGames games))

partx :: [Game] -> Int
partx = foldl' f 0
  where
    f acc game
      | isNothing x = acc
      | otherwise = acc + 3 * a + b
       where
         x = checkGame game
         (a, b) = fromMaybe errPart1 x
         errPart1 = error "Error: part1 not a Just" -- not reach

checkGame :: Game -> Maybe (Int, Int)
checkGame game
  | delta == 0 || ax == 0 = Nothing
  | denominator u == 1
    && denominator v == 1 = Just (a, b)
  | otherwise             = Nothing
  where
    (ax, ay) = aBut game
    (bx, by) = bBut game
    (px, py) = prize game
    delta = ax * by - ay * bx
    v = (ax * py - ay * px) % delta
    u = ((px  % 1) - (bx % 1) * v) * (1 % ax)
    a = numerator u
    b = numerator v

correctsGames :: [Game] -> [Game]
correctsGames = map f
  where
    c = 10000000000000
    f game = game {prize=(a+c, b+c)}
      where (a, b) = prize game

-- parsing stuff
parse :: ReadP a -> ReadS a
parse = readP_to_S

number :: ReadP Int
number = read <$> munch1 isDigit

twoNewLines :: ReadP ()
twoNewLines = void (string "\n\n")

newLine :: ReadP ()
newLine = void (char '\n')

spaces :: ReadP ()
spaces = skipMany1 (char ' ')

parseDatas :: String -> [Game]
parseDatas str =
  case parse readDatas str of
    [x] -> fst x
    []  -> error "Error: parseDatas: can't parse."
    _   -> error "Error: parseDatas: there are more than one result."

readDatas :: ReadP [Game]
readDatas = do
  games <- sepBy1 readGame twoNewLines
  optional newLine
  eof
  pure games

readGame :: ReadP Game
readGame = do
  abutton <- readButton
  newLine
  bbutton <- readButton
  newLine
  goal <- readPrize
  pure (Game {aBut = abutton
             ,bBut = bbutton
             ,prize = goal
             })

readButton :: ReadP (Int, Int)
readButton = do
  void (string "Button")
  spaces
  void (char 'A' <++ char 'B')
  void (char ':')
  spaces
  void (string "X+")
  x <- number
  void (char ',')
  spaces
  void (string "Y+")
  y <- number
  pure (x, y)

readPrize :: ReadP (Int, Int)
readPrize = do
  void (string "Prize:")
  spaces
  void (string "X=")
  x <- number
  void (char ',')
  spaces
  void (string "Y=")
  y <- number
  pure (x, y)
