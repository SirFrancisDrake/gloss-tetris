
module Draw where

import Control.Concurrent.STM
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Geometry

import Configure
import Display
import Tetriminos
import World

fx x = if odd game_width   then x else x - 0.5
fy y = if odd game_height  then y else y + 0.5

renderChWith :: (Square -> Picture) -> Chunk -> Picture
renderChWith fn ch = 
  let adjToLines s@(v0,_) ss = 
        map ( \(v1,_) -> scale const_t_side const_t_side $ line [v0,v1] ) 
            ( adjacentInDirections s ss [DUp, DRight])
      renderLines = map (flip adjToLines ch) ch
  in  Pictures $ (map fn ch) ++ [Translate (fx $ -0.5 * const_t_side) (fy 0) 
                                  $ Pictures $ concat renderLines]

renderT (T (posX, posY) k ls) = Pictures $ 
  map (\(x,y) -> renderS ((posX + x, posY + y), stColor k)) ls

renderS :: Square -> Picture
renderS ((posX, posY),c) = 
  let tr = Translate (fx posX * const_t_side) (fy posY * const_t_side) 
      trace = if trace_square_coords 
                then [ Translate (-10) (-10) $ scale 0.06 0.06 $ 
                       Text $ show (round posX) ++ ", " 
                           ++ show (round posY) ]
                else []
  in  tr $ Pictures $ (square const_t_side c):trace

renderSBlink :: [Int] -> Square -> Picture
renderSBlink blinkingLines s@((x,y),c) = 
  if round y `elem` blinkingLines
    then renderS ((x,y), light c)
    else renderS s

renderSGhost :: Square -> Picture
renderSGhost ((x,y), c) =
  let (r, g, b, _) = rgbaOfColor $ dark c
  in  renderS ((x,y), (makeColor r g b 0.2))

fix' fn x = if fn x == x then x else fix' fn (fn x)

draw World{ w_currentTetrimino = t
          , w_chunks = chs 
          , w_gameOver = tgo
          , w_displayState = ds 
          }
  | Playing <- ds = do
      go <- readTVarIO tgo -- game over
      let got = if go then gameOverSign else Blank
      p <- render (Just t) chs plainRender
      return $ Pictures $ [p, got]
  | Blinking{ d_blinkingTick = bt
            , d_iterationsLeft = its 
            , d_linesToBlink = lns 
            } <- ds
  , even its 
    = render Nothing chs (flip blinkRender lns)
  | otherwise = render Nothing chs plainRender

render :: Maybe Tetrimino -> [Chunk] -> ([Chunk] -> Picture) -> IO Picture
render mt chs rfn = 
  let mbRender fn 
        | Just t  <- mt = fn t
        | Nothing <- mt = Blank
  in  return $ Translate 0 (fy $ 0.5) $ 
        Pictures $ (rfn chs):(mbRender renderT):(mbRender (flip renderGhost chs)):[]

plainRender :: [Chunk] -> Picture
plainRender chs = Pictures $ map (renderChWith renderS) chs

blinkRender :: [Chunk] -> [Int] -> Picture
blinkRender chs is = Pictures $ map (renderChWith $ renderSBlink is) chs

renderGhost :: Tetrimino -> [Chunk] -> Picture
renderGhost t chs =
  let nt = fix' (\t -> moveT t (concat chs) DDown) t
  in  renderChWith renderSGhost (breakToSquares nt)

gameOverSign :: Picture
gameOverSign =  scale 0.2 0.2 $ Translate (- const_t_side * 10) 0 $ 
 Pictures [ Text "Game over"
          , color red $
            Polygon [ (-const_t_side * 10, const_t_side * 2)
                    , (-const_t_side *  1, const_t_side * 2)
                    , (-const_t_side *  1, -const_t_side * (-2))
                    , (-const_t_side * 11, -const_t_side * (-2))
                    ] ]
  
square :: Float -> Color -> Picture
square side c =
  let a = side/2
      p = Polygon [ (-a,  a)
                  , ( a,  a)
                  , ( a, -a)
                  , (-a, -a) ]
      path = color red $ lineLoop $ rectanglePath side side
  in  Pictures [color c p, path]
