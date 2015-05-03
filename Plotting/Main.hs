module Main where

import Sokoban (drawWorld, sokoWorld1)
import Harbors

import Diagrams.Prelude
import Diagrams.Backend.SVG

main :: IO ()
main = do
    renderSVG "soko1.svg" (dims2D 200 200) (drawWorld sokoWorld1)
    renderSVG "plot.svg" (dims2D 200 200) (example1 <> square 3)
