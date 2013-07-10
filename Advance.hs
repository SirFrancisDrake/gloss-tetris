
module Advance where

import Data.Function (fix, on)
import Data.List ((\\), intersect, sortBy)
import System.Random

import Configure
import Tetriminos

linesToClear :: [Chunk] -> [Int]
linesToClear chs =
  let squares = concat chs
  in  filter (\i -> length (filter (isSquareInRange [i]) squares) == game_width)
             [game_bottom..game_top]

isSquareInRange :: [Int] -> Square -> Bool
isSquareInRange is ((x,y),_) =
  and [ round y `elem` is
      , round x >= game_left
      , round x <= game_right ]

-- throws out all squares with line numbers in given range from given chunks
filterChunks :: [Chunk] -> [Int] -> [Chunk]
filterChunks chs is = 
  let fn = filter (not . (isSquareInRange is))
  in  map fn chs

-- moves everything down, chunk by chunk
-- very slowly, but it should work
dropChunks :: [Chunk] -> [Chunk]
dropChunks chs =
  let (bch, nbchs) = splitBorderChunk chs
      moveDown :: [Square] -> Chunk -> Chunk
      moveDown ss ch = if canMove ch (ss \\ ch) DDown
        then map (\(v,c) -> (v + directionV DDown, c)) ch
        else ch
      fixDown :: [Chunk] -> [Chunk]
      fixDown chs = 
        let ss = concat chs ++ bch
            nc = map (moveDown ss) chs
        in  if chs == nc then chs else fixDown nc
  in  bch:(fixDown nbchs)

fix' fn x = if fn x == x then x else fix' fn (fn x)
  
-- splits a chunk into interconnected components
splitChunk :: Chunk -> [Chunk]
splitChunk ch =
  let splitByAdj s ss = ( filter        (isAdjacent s)  ss
                        , filter (not . (isAdjacent s)) ss )
      srt = sortBy (on compare fst)
      resplit ss s =
        let (adj, nadj) = splitByAdj s ss
        in  (srt $ s:(concat adj)):(map srt nadj)
  in  foldl resplit [] ch

splitBorderChunk :: [Chunk] -> (Chunk, [Chunk])
splitBorderChunk chs =
  let p ch = (fromIntegral game_left - 1, fromIntegral game_bottom) 
                `elem` (map fst ch)
  in  (head $ filter p chs, filter (not . p) chs)

newFigure = do
  g <- newStdGen
  let (r,_) = randomR (0,6) g
      pos   = (0, fromIntegral game_top)
  return ( ([fig_i, fig_j, fig_l, fig_o, fig_s, fig_t, fig_z] !! r) pos )
