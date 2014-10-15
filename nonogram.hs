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

-- Nothing means we don't know the color yet
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

-- Computes list of placements for a single "slice"
slicePlacements :: Int -> Int -> [[Color]]
slicePlacements colorLen totalSpace = fmap f [0..(totalSpace - colorLen)]
  where f n = replicate n False ++ replicate colorLen True

-- Computes complete list of upossible placements for a set of slices
allPlacements :: Constraint -> Int -> [[Color]]
allPlacements [] len = [replicate len False]
allPlacements (x:xs) len =
  do
    start <- slicePlacements x len
    rest <- allPlacements xs (len - (length start) - 1)
    return $ take len $ start ++ [False] ++ rest

-- Tests if the known parts of a solution are compatible with a tentative
-- solution.
isCompatible :: Eq a => [Maybe a] -> [a] -> Bool
isCompatible [] [] = True
isCompatible [] _ = False
isCompatible _ [] = False
isCompatible (Nothing:xs) (_:ys) = isCompatible xs ys
isCompatible (Just x:xs) (y:ys) = x == y && isCompatible xs ys

-- Finds common elements between two lists.
commonElements :: Eq a => [Maybe a] -> [Maybe a] -> [Maybe a]
commonElements [] [] = []
commonElements (Nothing:xs) (_:ys) = Nothing : commonElements xs ys
commonElements (_:xs) (Nothing:ys) = Nothing : commonElements xs ys
commonElements (Just x:xs) (Just y:ys) =
  (if x == y then Just x else Nothing) : commonElements xs ys
commonElements _ _ = error "Mismatched list sizes"

-- Solves a row/column as much as possible independent of other rows/columns
solveRow :: Constraint -> Row -> Row
solveRow constraint row =
  foldl1' commonElements validPlacements where
    validPlacements =
      fmap (fmap Just) . filter (isCompatible row)
        $ allPlacements constraint (length row)

cellToChar :: Maybe Color -> Char
cellToChar Nothing = '?'
cellToChar (Just c) = if c then '■' else ' '

renderPuzzle :: Puzzle -> String
renderPuzzle p =
  intercalate "\n" [fmap cellToChar row | row <- rows p]

-- from puzzle-nonograms.com: 5x5 Nonograms Puzzle ID: 1,290,514
test1 = makePuzzle [[2],[3],[1],[1,3],[3]] [[4],[2],[1,2],[2],[2]]
