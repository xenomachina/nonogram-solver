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

type Row = [Maybe Color]

type Constraint = [Int]

type Color = Bool

makePuzzle :: [Constraint] -> [Constraint] -> Puzzle
makePuzzle rcs ccs =
  Puzzle {
    rowConstraints = rcs,
    colConstraints = ccs,
    rows = replicate height .  replicate width $ Nothing
  } where width = length ccs
          height = length rcs

cols :: Puzzle -> [[Maybe Color]]
cols = transpose . rows

slicePlacements :: Int -> Int -> [[Color]]
slicePlacements colorLen totalSpace = fmap f [0..(totalSpace - colorLen)]
  where f n = replicate n False ++ replicate colorLen True
