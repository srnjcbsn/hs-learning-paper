module Sokoban where

import           Diagrams.Backend.PGF
import           Diagrams.Prelude

data Object = Sokoban
            | Crate String

data Tile = Normal (Maybe Object)
          | Goal (Maybe Object)
          | Wall

type World = [[Tile]]

drawObject :: Maybe Object -> Diagram B
drawObject (Just Sokoban)   =  circle 0.3
drawObject (Just (Crate n)) =  square 0.6 # lw 2.0
                            <> baselineText ("$" ++ n ++ "$")
                               # fontSize (local 0.2)
drawObject  Nothing         =  mempty

drawTile :: Tile -> Diagram B
drawTile (Normal obj) =  square 1.0
                      <> drawObject obj
drawTile (Goal   obj) =  square 1.0
                      <> square 0.7 # dashingN [0.03,0.03] 0
                      <> drawObject obj
drawTile  Wall        = mempty

drawWorld :: World -> Diagram B
drawWorld = vcat . (map (hcat . (map drawTile))) # lw 2.0

tileLabels :: Int -> Int -> [Diagram B]
tileLabels start stop = map (dia . label) [start .. stop]
    where label n = "$t_{" ++ show n ++ "}$"
          dia t = strut 1.0 <> baselineText t # fontSize (local 0.2)

w, g, e, s :: Tile
w = Wall
g = Goal Nothing
e = Normal Nothing
s = Normal (Just Sokoban)

c :: String -> Tile
c = Normal . Just . Crate

sokoWorld1 :: World
sokoWorld1 = [ [ s,  c1, e, g ]
             , [ c2, w,  w, w ]
             , [ e,  w,  w, w ]
             , [ g,  w,  w, w ]
             ]
             where c1 = c "c_1"
                   c2 = c "c_2"

smallSokoWorld :: Diagram B
smallSokoWorld = drawWorld [ [g, c "c", s, e] ]
                 ===
                 hcat (tileLabels 1 4)

moveLeftBefore :: Diagram B
moveLeftBefore = drawWorld [ [e, c "c", s] ]
                 ===
                 hcat (tileLabels 1 3)

moveLeftAfter :: Diagram B
moveLeftAfter = drawWorld [ [c "c", s, e] ]
                ===
                hcat (tileLabels 1 3)

