{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
import CodeWorld

type DoorIsOpen = Bool

-- | Enumeration of types of tiles.
data Tile = Floor | Wall | Exit | Button Color | Door Color DoorIsOpen

-- | Render a single tile given its type as `Tile`.
drawTile :: Tile -> Picture
drawTile Floor = colored yellow (solidRectangle 0.95 0.95)
drawTile Wall = colored black (solidRectangle 0.95 0.95)
drawTile Exit = blank -- TODO
drawTile (Button color) = colored color (thickCircle 0.1 0.3) <> drawTile Floor
drawTile (Door color False) = colored color (solidCircle 0.3) <> drawTile Wall
drawTile (Door color True) = colored color (solidCircle 0.3) <> drawTile Floor

-- | Cartesian coordinates on an integer grid.
type Coords = (Integer, Integer)

-- | Level map type.
type LevelMap = (Coords -> Tile)

-- | Render the image of a player in the given coordinates.
renderPlayer :: Coords -> Picture
renderPlayer (dx, dy) =
  translated (fromIntegral dx) (fromIntegral dy) $ lettering "\x1F6B6"

renderTile :: LevelMap -> Coords -> Picture
renderTile levelMap (x, y) = translated dx dy (drawTile (levelMap (x, y)))
  where
    dx = fromIntegral x
    dy = fromIntegral y

renderFromTo :: (Integer -> Picture) -> Integer -> Integer -> Picture
renderFromTo renderFunc from to
  | from > to = blank
  | otherwise = renderFunc from <> renderFromTo renderFunc (from+1) to

renderRow :: LevelMap -> (Integer, Integer) -> Integer -> Picture
renderRow levelMap (from, to) y = renderFromTo (\x -> renderTile levelMap (x, y)) from to

-- | Representation of a world.
-- | Consists of a map and the current coordinates of the player.
data World = World LevelMap Coords

-- | Enumeration for directions: left, up, right, down.
data Dir = L | U | R | D

renderMap :: LevelMap -> Coords -> Coords -> Picture
renderMap levelMap (fromX, fromY) (toX, toY) =
  renderFromTo (renderRow levelMap (fromX, toX)) fromY toY

renderWorld :: World -> Coords -> Coords -> Picture
renderWorld (World levelMap (playerX, playerY)) (fromX, fromY) (toX, toY) =
  case levelMap (playerX, playerY) of
    Exit -> lettering "Hoooraaaay!!"
    _ -> renderPlayer (playerX, playerY) <> renderMap levelMap (fromX, fromY) (toX, toY)

-- | Calculate the new coordinates after a move.
coordsMove :: Coords -> Dir -> Coords
coordsMove (x, y) L = (x-1, y)
coordsMove (x, y) U = (x, y+1)
coordsMove (x, y) R = (x+1, y)
coordsMove (x, y) D = (x, y-1)

-- | Checks whether a player is allowed to enter the given tile.
isEnterable :: Tile -> Bool
isEnterable (Door _color isOpen) = isOpen
isEnterable Wall = False
isEnterable _tile = True

-- | Checks if the player can move in the given direction.
canMove :: World -> Dir -> Bool
canMove (World levelMap playerCoords) dir =
  isEnterable $ levelMap $ coordsMove playerCoords dir

-- | Invert states of the doors with the given color.
invertDoors :: Color -> LevelMap -> LevelMap
invertDoors invertC levelMap = levelMap'
  where
    levelMap' :: Coords -> Tile
    levelMap' coords =
      case levelMap coords of  -- Could I rewrite this part in a better way?
        Door curC isOpen -> Door curC (isOpen /= (curC == invertC))  -- xor...
        other -> other

-- | Activates the button the player is currently standing on (if any).
useButton :: World -> World
useButton (World levelMap playerCoords) = World levelMap' playerCoords
  where
    levelMap' =
      case levelMap playerCoords of
        Button c -> invertDoors c levelMap
        _ -> levelMap

-- | If possible, moves the player in the given direction and activates the
-- | button if they step on one.
tryMove :: Dir -> World -> World
tryMove dir world@(World levelMap playerCoords) =
  if canMove world dir then
    useButton (World levelMap (coordsMove playerCoords dir))
  else
    world

-- | Represents the map as a sequence of segments of tiles. Each element of the
-- | array consists of coordinates from, coordinates to and a tile that should
-- | appear in the given rectangle.
-- |
-- | If the rectangles of two (or more) elements of the array intersect, the
-- | first tile appearing in the array is considered (so the array can be read
-- | as a reversed redrawings sequence).
-- |
-- | WARNING: for each element both 'from' coordinates must be less or equal to
-- | 'to' coordinates (e.g. `(1, 2), (3, 4)` is ok, `(3, 2), (1, 4)` is an empty
-- | range)
mapAsSegments :: [(Coords, Coords, Tile)]
mapAsSegments =
  [
    ((-5, 6), (-5, 8), Door pink False),
    ((5, 6), (5, 8), Door pink False),
    ((-5, 6), (5, 6), Door pink False),
    
    ((-1, 1), (-1, 5), Wall),
    ((-4, -1), (1, -1), Wall),
    ((-4, -8), (-4, -3), Wall),

    ((-1, 0), (-1, 0), Door green False),
    ((0, 0), (0, 0), Exit),
    ((0, 1), (0, 1), Wall),
    ((-1, -1), (1, 1), Wall),
    
    ((6, -7), (6, -7), Button green),
    
    ((-1, -5), (-1, -5), Button green),
    ((0, -5), (0, -5), Door blue True),
    ((1, -5), (1, -5), Door green True),
    ((2, -5), (2, -5), Button blue),
    ((2, -4), (2, -4), Button pink),
    ((-1, -6), (2, -4), Floor),
    ((-2, -7), (3, -3), Wall),
    
    ((3, -6), (3, 2), Wall),
    ((3, -2), (8, -2), Wall),
    ((3, -7), (3, 3), Wall),
    ((3, -2), (8, -2), Wall),
    ((8, -5), (8, -2), Wall),
    ((7, -8), (7, -6), Wall),
    ((5, -8), (7, -8), Wall),
    ((5, -8), (5, -4), Wall),
    ((5, -4), (6, -4), Wall),
    
    ((-7, -6), (-7, -6), Door green True),
    ((-6, -7), (-6, -7), Door blue False),
    ((-7, -7), (-7, -7), Button green),
    ((-8, -8), (-6, -6), Wall),
    
    ((-9, -9), (9, 9), Floor)  -- All the rest is floor!
  ]

-- | Checks wheher the range formed by first two arguments contains the third
-- | argument. All boundaries included.
-- |
-- | WARNING: If `fromX > toX` or `fromY > toY`, the range is considered empty
-- | and the result is always `False`
isInRange :: Coords -> Coords -> Coords -> Bool
isInRange (fromX, fromY) (toX, toY) (x, y) = (fromX <= x && x <= toX)  &&
  (fromY <= y && y <= toY)

-- | Given the coordinates (3rd arg), finds the first tile associated with a
-- | range containing the given coordinates in the map-as-segments
-- | representation (2nd arg), or, if not found, returns the default tile based
-- | on the output of default drawer (1st arg).
findTileInSegments :: (Coords -> Tile) -> [(Coords, Coords, Tile)] -> Coords -> Tile
findTileInSegments defaultDrawer [] coords = defaultDrawer coords
findTileInSegments defaultDrawer ((from, to, tile):segs) coords =
  if isInRange from to coords then
    tile
  else
    findTileInSegments defaultDrawer segs coords

myLevelMap :: Coords -> Tile
myLevelMap = findTileInSegments chessDrawer mapAsSegments
  where
    chessDrawer (x, y) = if ((x + y) `mod` 2) == 0 then Floor else Wall

-- | The initial world representation.
initialWorld :: World
initialWorld = World myLevelMap (-9, 9)

worldUpdater :: Event -> World -> World
worldUpdater (KeyPress "W") = tryMove U
worldUpdater (KeyPress "A") = tryMove L
worldUpdater (KeyPress "S") = tryMove D
worldUpdater (KeyPress "D") = tryMove R
worldUpdater (KeyPress "E") = useButton
worldUpdater (KeyPress " ") = useButton
worldUpdater _ = id

main :: IO ()
main = activityOf initialWorld worldUpdater (\w -> renderWorld w (-11, -11) (11, 11))
--main = drawingOf $ renderWorld (invertDoors pink $ invertDoors green initialWorld) (-11, -11) (11, 11)
