
{-# LANGUAGE PatternGuards #-}

module Main where

import Control.Concurrent.STM
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Geometry.Line

import Advance
import Configure
import Draw
import Tetriminos
import World

react :: Event -> World -> IO World
react e w 
  | (EventKey (SpecialKey KeySpace) Down _ _) <- e = rotateIOW w
  | (EventKey (SpecialKey KeyRight) Down _ _) <- e = moveIOW w DRight
  | (EventKey (SpecialKey KeyLeft ) Down _ _) <- e = moveIOW w DLeft
  | (EventKey (SpecialKey KeyUp   ) Down _ _) <- e 
  , Up_MoveUp <- w_upLock w                      = moveIOW w DUp
  | (EventKey (SpecialKey KeyUp   ) Down _ _) <- e 
  , Up_DropLock False <- w_upLock w              
      = return w{ w_upLock = Up_DropLock True}
  | (EventKey (SpecialKey KeyDown ) Down _ _) <- e
      = atomically (writeTVar (w_downPressedT w) True) >> return w
  | (EventKey (SpecialKey KeyDown ) Up _ _)   <- e
      = atomically (writeTVar (w_downPressedT w) False) >> return w
  | otherwise = return w

rotateIOW w@World{ w_currentTetrimino = t, w_chunks = chs }
  = return w{ w_currentTetrimino = rotate' t (concat chs) }

moveIOW w@World{ w_currentTetrimino = t, w_chunks = chs } d
  = return w{ w_currentTetrimino = moveT t (concat chs) d }

main = initWorld >>= \w ->
  playIO 
    (InWindow "YetAnotherTetris" 
              (game_width * (floor const_t_side) + 1, game_height * (floor const_t_side) + 1) 
              (30, 30))
    (makeColor 0.9 0.7 0 1) 
    ticks_in_second -- ticks per second
    w               -- initial state
    draw            -- drawing function :: initstate -> Picture
    react           -- reacting function
    advanceWorld    -- updating function :: _ -> _ -> initstate -> initstate

