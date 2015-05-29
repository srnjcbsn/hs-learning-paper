module Main where

import Sokoban (drawWorld, sokoWorld1, smallSokoWorld)
import Harbors
import HyperGraph

import Diagrams.Prelude
import Diagrams.Backend.PGF

main :: IO ()
main = do
    -- renderSVG "output/soko1.svg" (dims2D 200 200) (drawWorld sokoWorld1)
    -- renderSVG "output/sokoSmall.svg" (dims2D 200 200) (drawWorld smallSokoWorld)
    -- renderSVG "output/plot.svg" (dims2D 200 200) (example1 <> square 3)
    renderPGF "output/hg1.pgf" (dims2D 200 200) (drawHg hg1)
    renderPGF "output/hgEx2_1.pgf" (dims2D 200 200) (drawHg hgEx2_1)
    renderPGF "output/hgEx2_2.pgf" (dims2D 200 200) (drawHg hgEx2_2)
    renderPGF "output/isomorphic.pgf" (dims2D 200 200) (drawHg isomorphic)
    renderPGF "output/isomorphicReduced.pgf" (dims2D 200 200) (drawHg isomorphicReduced)
