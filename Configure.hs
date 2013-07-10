
module Configure where

const_t_side = 30 :: Float

const_windowHeight = 600 :: Int
const_windowWidth  = 800 :: Int

game_height = 22 :: Int
game_width  = 5 :: Int

game_bottom = -(floor $ fromIntegral game_height/2) :: Int
game_top    = 
  let c = if even game_height then 1 else 0
  in  (floor $ fromIntegral game_height/2) - c :: Int
game_left   = 
  let c = if even game_width then 1 else 0
  in  -(floor $ fromIntegral game_width/2) + c :: Int
game_right  = (floor $ fromIntegral game_width/2) :: Int

ticks_in_second = 1200 :: Int

blinks_total = 4  :: Int
blink_length = ticks_in_second `div` 3 :: Int

speed_downPressed = 12 :: Int
speed_upLock = ticks_in_second :: Int
-- Advance the board at every (speedTable i)th tick
speedTable :: Int -> Int
speedTable i = ticks_in_second `div` i

key_up_mode = Up_DropLock False

data KeyUpMode = 
  Up_Disabled
  | Up_MoveUp
  | Up_DropLock { up_d_locked :: Bool }
  deriving ()

debug_key_up = False
trace_square_coords = False
