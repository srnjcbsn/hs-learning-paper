module Graph where

import           Diagrams.Backend.PGF
import           Diagrams.Prelude

import           Control.Arrow        ((***))

type Vert = (String, Int, Int) -- (predName, argIdx, id)
type Pos = (Double, Double)

data EdgeType = Binding
              | Predicate

data Edge = Edge Vert Vert EdgeType

data Graph = Graph
    { verts :: [(Pos, Vert)]
    , edges :: [Edge]
    }

edgeLabel :: EdgeType -> String
edgeLabel Binding   = "$\\bullet$"
edgeLabel Predicate = "$\\times$"

drawLabel :: EdgeType -> Diagram B
drawLabel et = baselineText (edgeLabel et) # fontSize (local 0.2)
                                           # translate ((- 0.1) ^& (- 0.01))

showName :: Vert -> String
showName (name, n, _) = "$" ++ name ++ "_" ++ show n ++ "$"

vertex :: Vert -> Diagram B
vertex n =  circle 0.3 # named n # opacity 0
         <> baselineText (showName n) # fontSize (local 0.2)
                                      # translate ((- 0.1) ^& (- 0.01))

drawVerts :: [(Pos, Vert)] -> Diagram B
drawVerts ls = position $ map (p2 *** vertex) ls

labelBetween :: Vert -> Vert -> EdgeType -> Diagram B -> Diagram B
labelBetween n1 n2 et =
    withName n1 $ \b1 ->
    withName n2 $ \b2 ->
        atop (moveTo (lerp 0.5 (location b1) (location b2)) (drawLabel et))

edge :: Diagram B -> Edge -> Diagram B
edge dia (Edge n1 n2 et) = labelBetween n1 n2 et dia

-- connectOutside' opts n1 n2 dia
--     where opts = with & arrowHead .~ noHead
--                       & shaftStyle %~ lw thick

drawEdges :: [Edge] -> Diagram B -> Diagram B
drawEdges es dia = foldl edge dia es

drawGraph :: Graph -> Diagram B
drawGraph g = drawEdges (edges g) $ drawVerts (verts g)

tree :: Graph
tree = Graph
    { verts = [ ((2, 5), q1)
              , ((2, 4), p1)
              , ((2, 3), p2)
              , ((1, 2), g1)
              , ((1, 1), m1)
              , ((3, 2), m2)
              , ((3, 1), g2)
              ]
    , edges = [ Edge q1 p1 Binding
              , Edge p1 p2 Predicate
              , Edge p2 g1 Binding
              , Edge g1 m1 Binding
              , Edge p2 m2 Binding
              , Edge m2 g2 Binding
              ]
    } where q1 = ("q", 1, 1)
            p1 = ("p", 1, 2)
            p2 = ("p", 2, 3)
            g1 = ("g", 1, 4)
            g2 = ("g", 1, 5)
            m1 = ("m", 1, 6)
            m2 = ("m", 1, 7)

graph :: Graph
graph = Graph
    { verts = [ ((2, 5), q1)
              , ((2, 4), p1)
              , ((2, 3), p2)
              , ((1, 2), g1)
              , ((3, 2), m1)
              ]
    , edges = [ Edge q1 p1 Binding
              , Edge p1 p2 Predicate
              , Edge p2 g1 Binding
              , Edge p2 m1 Binding
              , Edge g1 m1 Binding
              , Edge m1 g1 Binding
              ]
    } where q1 = ("q", 1, 1)
            p1 = ("p", 1, 2)
            p2 = ("p", 2, 3)
            g1 = ("g", 1, 4)
            m1 = ("m", 1, 6)
