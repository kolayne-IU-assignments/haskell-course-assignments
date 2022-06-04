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

-- | Interactive finite state machine simulation.
interactiveFSM
  :: s                  -- ˆ Initial state.
  -> (a -> a -> Bool)   -- ˆ Action equality test.
  -> (s -> [(a, s)])    -- ˆ State transitions.
  -> (Event -> Maybe a) -- ˆ How to convert events into actions.
  -> (s -> Picture)     -- ˆ How to draw states.
  -> (a -> Picture)     -- ˆ How to draw actions.
  -> IO ()
interactiveFSM initS actionEq transitFunc event2Act pictureState pictureAct
  = activityOf initS eventProcessor drawer
  where
    eventProcessor event state
      = applyAction (event2Act event) actionEq transitFunc state
    
    drawer s = asSpaced 4 id ((pictureState s):(Prelude.map (pictureAct . fst) $ transitFunc s))

interactiveSystem
  :: s                                  -- ˆ Initial state of a FSM.
  -> (a -> a -> Bool)                   -- ˆ FSM action equality test.
  -> (s -> [(a, s)])                    -- ˆ FSM State transitions.
  -> (Event -> Maybe a)                 -- ˆ How to convert events into actions.
  -> (s -> Picture)                     -- ˆ How to draw states.
  -> (a -> Picture)                     -- ˆ How to draw actions.
  -> system                             -- ˆ System state, whose modes are
                                        -- modelled with FSM.
  -> (Double -> s -> system -> system)  -- ˆ How system evolves with time.
  -> (system -> Picture)                -- ˆ How to render system.
  -> IO ()
interactiveSystem initS actionEq transitFunc event2Act pictureState pictureAct
    initSys evolveSys pictureSys
  = activityOf (initS, initSys) eventProcessor drawer
  where
    eventProcessor event (state, sys)
      = (applyAction (event2Act event) actionEq transitFunc state, tryEvolve event state sys)
    
    tryEvolve (TimePassing dt) state sys = evolveSys dt state sys
    tryEvolve _ _ sys = sys
    
    drawer (st, sys) = asSpaced 4 id ((pictureSys sys):(pictureState st):(Prelude.map (pictureAct . fst) $ transitFunc st))

-- | Elevator system state: the current height
type ElevatorSystem = Double

initialSystem :: ElevatorSystem
initialSystem = 0

drawSystem :: ElevatorSystem -> Picture
drawSystem height = translated 0 height $ rectangle 1 2 <> lettering "\x1F6B6"

elevatorVelocity :: Double
elevatorVelocity = 1

evolveSystem :: Double -> Mode -> ElevatorSystem -> ElevatorSystem
evolveSystem _dt Idle height = height
evolveSystem dt GoingUp height = height + dt * elevatorVelocity
evolveSystem dt GoingDown height = height - dt * elevatorVelocity

main :: IO ()
main = interactiveSystem Idle buttonEq elevator elevatorEventer drawMode
  drawButton initialSystem evolveSystem drawSystem
  where
    elevatorEventer (KeyPress "Up") = Just Up
    elevatorEventer (KeyPress "Down") = Just Down
    elevatorEventer (KeyPress " ") = Just Stop
    elevatorEventer _ = Nothing
