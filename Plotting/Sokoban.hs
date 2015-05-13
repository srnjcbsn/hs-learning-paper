module Sokoban where

import Diagrams.Prelude
import Diagrams.Backend.SVG

data Object = Sokoban
            | Crate

data Tile = Normal (Maybe Object)
          | Goal (Maybe Object)
          | Wall

type World = [[Tile]]

drawObject :: Maybe Object -> Diagram B
drawObject (Just Sokoban) = circle 0.3
drawObject (Just Crate)   = square 0.6 # lw 2.0
drawObject  Nothing       = mempty

drawTile :: Tile -> Diagram B
drawTile (Normal obj) =  square 1.0
                      <> drawObject obj
drawTile (Goal   obj) =  square 1.0
                      <> square 0.7 # dashingN [0.03,0.03] 0
                      <> drawObject obj
drawTile  Wall        = mempty

drawWorld :: World -> Diagram B
drawWorld = vcat . (map (hcat . (map drawTile))) # lw 2.0

w, g, c, e, s :: Tile
w = Wall
g = Goal Nothing
c = Normal (Just Crate)
e = Normal Nothing
s = Normal (Just Sokoban)

sokoWorld1 :: World
sokoWorld1 = [ [ s, c, e, g ]
             , [ c, w, w, w ]
             , [ e, w, w, w ]
             , [ g, w, w, w ]
             ]

smallSokoWorld :: World
smallSokoWorld = [ [g, c, s, e] ]
