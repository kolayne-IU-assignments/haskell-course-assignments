{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text

-- | Elevator button
data Button = Up | Down | Stop

-- | Elevator mode of operation.
data Mode = Idle | GoingUp | GoingDown

-- | Checks whether two buttons are the same.
buttonEq :: Button -> (Button -> Bool)
buttonEq Up Up = True
buttonEq Down Down = True
buttonEq Stop Stop = True
buttonEq _ _ = False

elevator :: Mode -> [(Button, Mode)]
elevator Idle      = [(Up, GoingUp), (Down, GoingDown)]
elevator GoingUp   = [(Stop, Idle)]
elevator GoingDown = [(Stop, Idle)]

-- | Apply action for an FSM
applyAction
  :: Maybe a  -- ^ Action, if any
  -> (a -> a -> Bool)  -- ^ Equality checker
  -> (s -> [(a, s)])  -- ^ State transitions
  -> s  -- ^ Current state
  -> s  -- ^ New state
applyAction Nothing _ _ s = s
applyAction (Just action) eq transitor curState
  = applyAction' action eq (transitor curState)
  where
    applyAction' :: a -> (a -> a -> Bool) -> [(a, s)] -> s
    -- Considering attempts to perform an invalid transition as fatal, thus,
    -- panicing. If wanted to treat invalid transitions as inaction,
    -- `applyAction'` would have accepted the current state as the last argument
    -- and return it instead of `undefined`
    applyAction' _ _ [] = undefined
    applyAction' action eq ((a1, s1):transitions) =
      if eq action a1 then
        s1
      else
        applyAction' action eq transitions

-- | Base for the Elevator button.
buttonBase :: Picture
buttonBase = solidCircle 1.6

-- | Render text on the Elevator button base.
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

-- | Elevator mode base picture.
modeBase :: Picture
modeBase = colored (light green) $ solidRectangle 3 4

modePartUp :: Picture
modePartUp = translated 0 1.2 (lettering "Up")

modePartDown :: Picture
modePartDown = translated 0 (-1.2) (lettering "Down")

drawMode :: Mode -> Picture
drawMode Idle = modePartUp <> modePartDown <> modeBase
drawMode GoingUp = colored red modePartUp <> modePartDown <> modeBase
drawMode GoingDown = modePartUp <> colored red modePartDown <> modeBase

main :: IO ()
main = activityOf Idle eventProcessor drawMode
  where
    eventProcessor (KeyPress "Up") mode = applyAction (Just Up) buttonEq elevator mode
    eventProcessor (KeyPress "Down") mode = applyAction (Just Down) buttonEq elevator mode
    eventProcessor (KeyPress " ") mode = applyAction (Just Stop) buttonEq elevator mode
    eventProcessor _ x = x
-- main = drawingOf $ asSpaced 3.5 id [drawButton Up, drawButton Down, drawButton Stop]
-- main = drawingOf $ drawMode GoingDown
