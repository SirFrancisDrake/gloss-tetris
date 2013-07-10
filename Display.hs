
module Display where

data GameDisplayState = 
  Playing 
  | Blinking
    { d_blinkingTick   :: Int
    , d_iterationsLeft :: Int
    , d_linesToBlink   :: [Int]
    }
  deriving ()
