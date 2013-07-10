
{-# LANGUAGE PatternGuards #-}

module Tetriminos where

import Data.List (intersect)
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

type Square = (Vector, Color)
type Chunk  = [Square]

data Tetrimino = T
  { t_vec :: Vector 
  , t_kind :: Kind 
  , t_local :: [Vector]
  }
  -- pos   type  local
  deriving (Eq)

data Kind =
  IKind
  | JKind
  | LKind
  | OKind
  | SKind
  | TKind
  | ZKind
  deriving (Eq)

stColor :: Kind -> Color
stColor IKind = (makeColor (195/255) (33/255) (72/255) 1) -- maroon
stColor JKind = white
stColor LKind = magenta
stColor OKind = (makeColor 0 0 (40/255) 1) -- dark blue
stColor SKind = green
stColor TKind = (makeColor (226/255) (202/255) (202/255) 1) -- brown
stColor ZKind = cyan

fig_i pos = T pos IKind [(-1,0), (0,0), (1,0), (2,0)]
fig_j pos = T pos JKind [(-1,0), (0,0), (1,0), (1,-1)]
fig_l pos = T pos LKind [(-1,-1), (-1,0), (0,0), (1,0)]
fig_o pos = T pos OKind [(-1,0), (0,0), (0,-1), (-1,-1)]
fig_s pos = T pos SKind [(-1,0), (0,0), (0,1), (1,1)]
fig_t pos = T pos TKind [(-1,0), (0,0), (1,0), (0,1)]
fig_z pos = T pos ZKind [(-1,1), (0,0), (0,1), (1,0)]

rotateT t@(T pos _ _) = 
  let rt (T p k ls) = T p k (map (\(x,y) -> (y,-x)) ls)
      specialRotate f = if t == f pos then rt t else f pos
  in  case t_kind t of
        SKind -> specialRotate fig_s
        ZKind -> specialRotate fig_z
        otherwise -> rt t

rotate' :: Tetrimino -> [Square] -> Tetrimino
rotate' t@(T p k ls) ss' =
  let ss = map fst ss'
      adjacent = intersect ss $ concatMap (\v -> map (+v) (map fst $ breakToSquares t)) [(0,1),(1,0),(0,-1),(-1,0)]
      lines = map (\(x,y) -> (p + (x,y), p + (y,-x))) ls
      fn ((x1,y1), (x2,y2))
        | x1 > x2 && y1 < y2 = [(x, y) | x <- [x2..(x1-1)], y <- [y1..(y2-1)]]
        | x1 > x2 && y1 > y2 = [(x, y) | x <- [(x2+1)..x1], y <- [y2..(y1-1)]]
        | x1 < x2 && y1 < y2 = [(x, y) | x <- [x1..(x2-1)], y <- [(y1+1)..y2]]
        | x1 < x2 && y1 > y2 = [(x, y) | x <- [(x1+1)..x2], y <- [(y2+1)..y1]]
    -- moving clockwise between xs, want to hit the corners
    --             *-x-*
    --      x<,y<  |/ \|  x<,y>
    --             x   x
    --      x>,y<  |\ /|  x>,y>
    --             *-x-*
        | otherwise = []
      intermediateEmpty = null $ intersect ss $ concatMap fn lines
      positionEmpty = null $ intersect ss $ map fst $ breakToSquares $ rotateT t
  in if positionEmpty && intermediateEmpty
    then rotateT t
    else t

breakToSquares :: Tetrimino -> Chunk
breakToSquares (T p k ls) = map (\v -> (v+p, stColor k)) ls

adjacent s ss = adjacentInDirections s ss [DUp, DLeft, DDown, DRight]
     
adjacentInDirections (v,c) ss dirs = intersect (map (\d -> (v + directionV d, c)) dirs) ss

isAdjacent s ss = not $ null $ adjacent s ss

data Direction = DUp | DRight | DDown | DLeft
  deriving (Eq)

directionV DUp    = ( 0,  1 )
directionV DRight = ( 1,  0 )
directionV DDown  = ( 0, -1 )
directionV DLeft  = (-1,  0 )

canMove :: Chunk -> [Square] -> Direction -> Bool
canMove ls ss' d =
  let ss = map fst ss'
      v = directionV d
  in  null $ intersect ss $ map (+v) (map fst ls)

moveT :: Tetrimino -> [Square] -> Direction -> Tetrimino
moveT t@(T p k ls) ss' d =
  let ss = map fst ss'
      v = directionV d
  in  if not $ null $ intersect ss $ map (+v) (map fst $ breakToSquares t)
        then t
        else T (p + v) k ls

