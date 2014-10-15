-- A Nonogram solver
-- by Laurence Gonsalves
--
-- This isn't meant to be a perfect solver. It's an attempt at implementing
-- essentially the same process I use when solving nonograms "by hand".
--
-- Also, I'm writing this to learn Haskell, so my code might be
-- weird/non-idiomatic.

{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List

data Puzzle = Puzzle {
  rowConstraints :: [Constraint],
  colConstraints :: [Constraint],
  rows :: [Row]
} deriving Show

type Row = [Maybe Bool]

type Constraint = [Int]

makePuzzle :: [Constraint] -> [Constraint] -> Puzzle
makePuzzle rcs ccs =
  Puzzle {
    rowConstraints = rcs,
    colConstraints = ccs,
    rows = take (length rcs) . repeat .
           take (length ccs) . repeat $ Nothing
  }

cols :: Puzzle -> [[Maybe Bool]]
cols = transpose . rows
