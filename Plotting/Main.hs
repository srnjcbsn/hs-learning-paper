module Main where

import Sokoban (drawWorld, sokoWorld1, smallSokoWorld)
import Harbors

import Diagrams.Prelude
import Diagrams.Backend.SVG

main :: IO ()
main = do
    renderSVG "output/soko1.svg" (dims2D 200 200) (drawWorld sokoWorld1)
    renderSVG "output/sokoSmall.svg" (dims2D 200 200) (drawWorld smallSokoWorld)
    renderSVG "output/plot.svg" (dims2D 200 200) (example1 <> square 3)
