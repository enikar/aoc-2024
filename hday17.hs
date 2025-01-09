-- AoC 2024, day 17, part1 naive solution

{- For part1 it is ok. For part2 the order of magnitude
   is too high to brute force the solution with my
   computer.

  My input program involve this sequence of
  operations:

  2,4 bst: A & 7 -> B
  1,2 bxl: B `xor` 2 -> B
  7,5 cdv: A `quot` (2^B) -> C
  1,3 bxl: B `xor` 3 -> B
  4,3 bxc: B `xor` C -> B
  5,5 out: emit (B & 7)
  0,3 adv: A `quot` (2^3) -> A
  3,0 jnz: jump to 0 if (A /= 0)

  So, the initial value of A would be between 8^15 and 8^16
  since we need to loop 16 times, and A is divided by 8 on
  each loop. Then for part2 we need to initialize register
  A with 8^15 There are yet many numbers to test:
  8^16 = 2.8e14 and 8^15 = 3.5e13

  My computer can only check 335*10^6 numbers per hour with
  this naive solution. Then it is impossible to solve this
  puzzle in this way.
  Indeed at this speed we'll need more 83 years to check all
  numbers between 8^15 and 8^16, and we don't know where the
  solution could be found in this range.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}
-- {-# LANGUAGE NumDecimals #-}

{- HLINT ignore "Eta reduce" -}

module Main (main) where

-- import Debug.Trace (trace)
import Data.Array.Unboxed
  (UArray
  ,(!)
  )
import Data.Array.Unboxed qualified as UArray
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MapS
import Data.Bits
  (xor
  ,(.&.)
  ,shiftR
  )
import Data.Functor (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import Data.Attoparsec.ByteString.Char8
  (Parser
  ,string
  ,parseOnly
  ,endOfLine
  ,sepBy1'
  ,satisfy
  ,decimal
  ,char
  )

data Computer = CP {memory :: UArray Int Int
                   ,pc :: Int
                   ,registers :: Map Char Int
                   ,emited :: [Int]
                   ,elen :: Int
                   } deriving (Show)

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  cp <- getDatas "day17.txt"
  printSolution "Part1" (emited (part1 cp))
  printSolution "Part2" (part2 cp)

part1 :: Computer -> Computer
part1 cp = until halt step cp
  where
    (_, sup0) = UArray.bounds (memory cp)
    sup = sup0 - 1

    halt cp' = pc0 == sup && regA == 0
      where
        pc0 = pc cp'
        regA = registers cp' MapS.! 'A'

-- My naive solution for part2 doesn't work.
-- Using Data.Map.Strict solve the leak issue.
part2 :: Computer -> Int
part2 cp0 = go a0 cp0'
  where
    goal = UArray.elems (memory cp0)
    glen = length goal
    a0 = 8 ^ (15 :: Int)

    cp0' = cp0 {registers = MapS.insert 'A' a0 regs}
      where
        regs = registers cp0

    (_, sup0) = UArray.bounds (memory cp0)
    sup = sup0 - 1

    go a cp
      | pc0 == sup
        && regA == 0
        && glen == clen
        && goal == emited cp         = a
      | clen >= glen
        || (pc0 == sup && regA == 0) = go a' cp'
      | otherwise                    = go a (step cp)
        where
          pc0 = pc cp
          clen = elen cp
          regs = registers cp
          regA = regs MapS.! 'A'
          a' = a+1
          -- a' = let a'' = a+1
          --      in if a'' `rem` 1e6 == 0
          --         then trace ("Value of register A: " <> show a'') a''
          --         else a''

          cp' = cp0 {registers = MapS.insert 'A' a' regs}


step :: Computer -> Computer
step cp = (instructions !! inst) cp
  where
    pc0 = pc cp
    inst = memory cp ! pc0

instructions :: [Computer -> Computer]
instructions = [opAdv -- 0
               ,opBxl -- 1
               ,opBst -- 2
               ,opJnz -- 3
               ,opBxc -- 4
               ,opOut -- 5
               ,opBdv -- 6
               ,opCdv -- 7
               ]

-- instructions
combo :: Int -> Computer -> Int
combo n cp
  | n `elem` [0..3] = n
  | otherwise       = registers cp MapS.! reg
  where
    reg = "ABC" !! (n-4)

opXdv :: Char -> Computer -> Computer
opXdv reg cp = cp {pc = pc0+2, registers = MapS.insert reg val regs}
  where
    pc0 = pc cp
    mem = memory cp
    cb = combo (mem ! (pc0+1)) cp
    regs = registers cp
    regA = regs MapS.! 'A'
    -- val = regA `quot` (1 `shiftL` cb) -- truncated division
    -- dividing by a power of two is equal to shifting with the
    -- same exponent to the right
    val = regA `shiftR` cb

-- opcode 0 = adv
opAdv :: Computer -> Computer
opAdv = opXdv 'A'

-- opcode 1 = bxl
opBxl :: Computer -> Computer
opBxl cp = cp {pc = pc0 + 2, registers = MapS.insert 'B' val regs}
  where
    pc0 = pc cp
    mem = memory cp
    lit = mem ! (pc0 + 1)
    regs = registers cp
    regB = regs MapS.! 'B'
    val = regB `xor` lit

-- opcode 2 = bst
opBst :: Computer -> Computer
opBst cp = cp {pc = pc0 + 2, registers = MapS.insert 'B' cb regs}
  where
    pc0 = pc cp
    mem = memory cp
    cb = combo  (mem ! (pc0 + 1)) cp .&. 7
    regs = registers cp

-- opcode 3 = jnz
opJnz :: Computer -> Computer
opJnz cp {- -- | regA == 0 && pc0 >= 14 = errorJnz -}
         {- -- | regA == 0 = cp {pc = pc0 + 2} -}
        = cp {pc = lit}
  where
    pc0 = pc cp
    mem = memory cp
    --regs = registers cp
    --regA = regs MapS.! 'A'
    lit = mem ! (pc0 + 1)
    -- errorJnz = error ("Error: jnz: " <> show cp)

-- opcode 4 = bxc
opBxc :: Computer -> Computer
opBxc cp = cp {pc = pc0 + 2, registers = MapS.insert 'B' val regs}
  where
    pc0 = pc cp
    -- mem = memory cp
    regs = registers cp
    regB = regs MapS.! 'B'
    regC = regs MapS.! 'C'
    val = regB `xor` regC

-- opcode 5 = out
opOut :: Computer -> Computer
opOut cp = cp {pc = pc0 + 2, emited = outs, elen = n+1}
  where
    pc0 = pc cp
    mem = memory cp
    n = elen cp
    -- n' | n >= 16   = error ("Error: out: " <> show cp)
    --    | otherwise = n+1
    cb = combo (mem ! (pc0 + 1)) cp .&. 7
    outs = emited cp ++ [cb]

-- opcode 6 = bdv
opBdv :: Computer -> Computer
opBdv = opXdv 'B'

-- opcode 7 = cdv
opCdv :: Computer -> Computer
opCdv = opXdv 'C'

-- parsing and intializations
getDatas :: String -> IO Computer
getDatas filename = parseDatas <$> BS.readFile filename

parseDatas :: ByteString -> Computer
parseDatas str = CP {memory = mem
                    ,pc = 0
                    ,registers = regs
                    ,emited = []
                    ,elen = 0}
  where
    (mem, regs) = either error
                         id
                         (parseOnly parseComputer str)

parseComputer :: Parser (UArray Int Int, Map Char Int)
parseComputer = do
  regs <- parseRegisters
  void (string "\n\n")
  mem <- parseMemory
  pure (mem, regs)

parseRegisters :: Parser (Map Char Int)
parseRegisters = do
  regs <- sepBy1' parseReg endOfLine
  let buildRegs (reg, n) acc = MapS.insert reg n acc
      regs' = foldr buildRegs MapS.empty regs
  pure regs'

parseReg :: Parser (Char, Int)
parseReg = do
  void (string "Register ")
  reg <- satisfy (`elem` ("ABC" :: String))
  void (string  ": ")
  n <- decimal
  pure (reg, n)

parseMemory :: Parser (UArray Int Int)
parseMemory = do
  void (string "Program: ")
  instrs <- sepBy1' decimal (char ',')
  let sup = length instrs - 1
      arr = UArray.array (0, sup) (zip [0..] instrs)
  pure arr
