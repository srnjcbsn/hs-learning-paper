module Main where

import           Harbors
import           HyperGraph
import           Sokoban              (drawWorld, smallSokoWorld, sokoWorld1)

import           Diagrams.Backend.PGF
import           Diagrams.Prelude

import           System.FilePath

outPut :: FilePath
outPut = "../Graphics/"

draw :: String -> Diagram B -> IO ()
draw name dia = renderPGF (outPut </> name ++ ".pgf") (dims2D 200 200) dia

main :: IO ()
main = do
    -- renderSVG "output/soko1.svg" (dims2D 200 200) (drawWorld sokoWorld1)
    -- renderSVG "output/sokoSmall.svg" (dims2D 200 200) (drawWorld smallSokoWorld)
    -- renderSVG "output/plot.svg" (dims2D 200 200) (example1 <> square 3)
    draw "hg1"               (drawHg hg1)
    draw "hgEx2_1"           (drawHg hgEx2_1)
    draw "hgEx2_2"           (drawHg hgEx2_2)
    draw "isomorphic"        (drawHg isomorphic)
    draw "isomorphicReduced" (drawHg isomorphicReduced)
    draw "sokobanHyperGraph" (drawHg sokobanHg)
    draw "bindingEdge"       (drawHg bindingEdge)
    draw "predicateEdge"     (drawHg predicateEdge)
