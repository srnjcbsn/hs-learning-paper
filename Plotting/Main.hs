module Main where

import           Graph
import           Harbors
import           HyperGraph
import           Sokoban
import           Statistics

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
    draw "sokoSmall"         smallSokoWorld
    draw "moveLeftBefore"    moveLeftBefore
    draw "moveLeftAfter"     moveLeftAfter

    draw "sokoTrain1"        training1
    draw "sokoTrain2"        training2
    draw "sokoTrain3"        sokoWorld1

    draw "hg1"               (drawHg hg1)
    draw "hgEx1_1"           (drawHg hgEx1_1)
    draw "hgEx1_2"           (drawHg hgEx1_2)
    draw "hgEx2_1"           (drawHg hgEx2_1)
    draw "hgEx2_2"           (drawHg hgEx2_2)
    draw "isomorphic"        (drawHg isomorphic)
    draw "isomorphicReduced" (drawHg isomorphicReduced)
    draw "sokobanHyperGraph" (drawHg sokobanHg)
    draw "bindingEdge"       (drawHg bindingEdge)
    draw "predicateEdge"     (drawHg predicateEdge)
    draw "graphExample"      (drawHg graphExample)
    draw "hypUnproven"       (drawHg hypUnproven)
    draw "hypCands"          hypCandsD

    draw "tree"              (drawGraph tree)
    draw "graph"             (drawGraph graph)
    drawHist "statistics2" >>= mapM_ (uncurry draw)
    drawHist "statisticsLong" >>= mapM_ (uncurry draw)
