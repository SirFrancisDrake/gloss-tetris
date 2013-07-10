
module World where

import Control.Concurrent.STM
import Data.Function (on)
import Data.List (intersect)
import Graphics.Gloss

import Advance
import Configure
import Display
import Tetriminos

data World = World
  { w_currentTetrimino :: Tetrimino
  , w_chunks       :: [Chunk]
  , w_score        :: TVar Int
  , w_speed        :: TVar Int
  , w_downPressedT :: TVar Bool
  , w_upLock       :: KeyUpMode
  , w_tickNumber   :: TVar Int
  , w_gameOver     :: TVar Bool
  , w_displayState :: GameDisplayState
  }

initWorld :: IO World
initWorld = 
  let lbound = game_left -1
      rbound = game_right + 1
      tbound = game_top + 1
      bbound = game_bottom - 1
      f x y = ( (fromIntegral x, fromIntegral y), red )
      borders =
          [ [ f x y | x <- [lbound, rbound]
                    , y <- [bbound..tbound] ] 
              ++ 
            [ f x bbound | x <- [(lbound + 1)..(rbound - 1)]]
          ] 
  in  do tscore <- newTVarIO 0
         tspeed <- newTVarIO 1
         tdown  <- newTVarIO False
         t      <- newFigure
         tn     <- newTVarIO 0
         go     <- newTVarIO False
         let ds = Playing
         return $ World t borders tscore tspeed tdown key_up_mode tn go ds

-- Advance the board iff tickNumber `mod` (speedTable speed) == 0
advanceWorld _ w@World{ w_currentTetrimino = t
                      , w_chunks           = chs
                      , w_downPressedT     = td 
                      , w_upLock           = ul 
                      , w_speed            = tsp
                      , w_tickNumber       = ttn 
                      , w_gameOver         = tgo
                      , w_displayState     = dstate
                      } = do
  speed       <- readTVarIO tsp
  tickNumber  <- readTVarIO ttn
  downPressed <- readTVarIO td
  go          <- readTVarIO tgo
  if go then atomically (writeTVar ttn 1)
        else atomically (writeTVar ttn $ tickNumber + 1)
  let f n = tickNumber `mod` (speedTable n) == 0
      doWeAct | Up_DropLock True <- ul
                = (f speed) || (downPressed && (f speed_downPressed)) || (f speed_upLock)
              | otherwise = (f speed) || (downPressed && (f speed_downPressed))
      step nc = dropChunks $ concatMap splitChunk $ filterChunks nc $ linesToClear nc
      nchs = chs ++ [breakToSquares t]
      canPlace ss1 ss2 = null $ on intersect (map fst) ss1 ss2
      action | doWeAct
             , Playing <- dstate -- not paused to clear lines
             , not $ null $ linesToClear chs
               = return w{ w_displayState = 
                             Blinking{ d_blinkingTick = 1 
                                     , d_iterationsLeft = blinks_total
                                     , d_linesToBlink = linesToClear chs }
                         }
             | doWeAct
             , Playing <- dstate -- not paused to clear lines
             , null $ linesToClear chs
             , canMove (breakToSquares t) (concat chs) DDown
               = return w{ w_currentTetrimino = moveT t (concat chs) DDown }
             | doWeAct
             , Playing <- dstate -- not paused to clear lines
               = newFigure >>= \nf -> if canPlace (concat nchs) (breakToSquares nf)
                   then return w{ w_currentTetrimino = nf
                                , w_chunks = nchs 
                                , w_upLock = key_up_mode }
                   else atomically (writeTVar tgo True) >> return w
             | Blinking{ d_blinkingTick = bt 
                       , d_iterationsLeft = 0
                       } <- dstate
             , bt `mod` blink_length == 0 -- just finished blinking
               =  return w{ w_chunks = step chs 
                          , w_displayState = Playing }
             | b@Blinking{ d_blinkingTick = bt 
                         , d_iterationsLeft = il
                         } <- dstate
             , bt `mod` blink_length == 0 -- just finished an iteration
               =  return w{ w_displayState = b{ d_iterationsLeft = il - 1
                                              , d_blinkingTick = bt + 1 }
                          }
             | b@Blinking{ d_blinkingTick = bt 
                         , d_iterationsLeft = il
                         } <- dstate
               =  return w{ w_displayState = b{ d_blinkingTick = bt + 1 } }
             | otherwise
               = return w
  if not go then action
            else return w
