{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text

-- | Elevator button
data Button = Up | Down | Stop

buttonBase :: Picture
buttonBase = solidCircle 1.6

drawButtonText :: Text -> Picture
drawButtonText s = colored white (lettering s) <> buttonBase

-- | Render elevator button.
drawButton :: Button -> Picture
drawButton Up = drawButtonText "UP"  -- "↑"
drawButton Down = drawButtonText "DOWN"  -- "↓"
drawButton Stop = drawButtonText "STOP"

-- | Draw several objects some distance apart from each other.
asSpaced
  :: Double         -- ˆ How far apart to draw objects.
  -> (a -> Picture) -- ˆ How to draw a single object.
  -> [a]            -- ˆ A list of objects to draw.
  -> Picture
asSpaced _ _ [] = blank
asSpaced distance func lst
  = translated (-badShift) 0 (asSpaced' distance $ Prelude.map func lst)
  where
    badShift = fromIntegral (Prelude.length lst - 1) * distance / 2
    
    asSpaced' :: Double -> [Picture] -> Picture
    asSpaced' _ [] = blank
    asSpaced' distance (pic:pics) = pic <> translated distance 0 (asSpaced' distance pics)

main :: IO ()
main = drawingOf $ asSpaced 3.5 id [drawButton Up, drawButton Down, drawButton Stop]
