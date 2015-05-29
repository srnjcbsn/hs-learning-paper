module HyperGraph where

import Diagrams.Prelude
import Diagrams.Backend.PGF
import Diagrams.TwoD.Offset

import Data.Map (Map, fromList, (!))
import Control.Arrow ((***))

type Vert = (String, Int, Int) -- (predName, argIdx, id)
type Pos = (Double, Double)

blobLW, blobR :: Double
blobLW = 0.02
blobR = 0.4

showName :: Vert -> String
showName (name, n, _) = "$" ++ name ++ "_" ++ show n ++ "$"

data HyperGraph = HyperGraph
    { verts :: Map Vert Pos
    , pSets :: [[Vert]]
    , bSets :: [[Vert]]
    }

vertex :: Vert -> Diagram B
vertex name =   circle 0.05 # fc black # named name
            <> baselineText (showName name)
                # fontSize (local 0.2)
                # translate (0.1 ^& 0.1)

drawBSet :: [Pos] -> Diagram B
drawBSet [b] =  moveTo (p2 b)
             $  circle (blobR - blobLW) # fc white # lc white
             <> circle blobR  # fc black

drawBSet bs = showSet where
    p = fromVertices $ map p2 bs
    e = expandTrail' opts blobR p
    f = expandTrail' opts (blobR - blobLW) p
    showSet =  f # strokePath # fc white # lc white
            <> e # strokePath # fc black # lc black
    opts = with & expandJoin .~ LineJoinRound
                & expandCap  .~ LineCapRound



connectList :: IsName a => [a] -> Diagram B -> Diagram B
connectList (a : b : t) d = connect' opts a b $ connectList (b : t) d where
    opts = with & arrowHead .~ noHead
                & shaftStyle %~ lw thick . lc black
connectList _ d = d

drawPSet :: [(Pos, Vert)] -> Diagram B
drawPSet ps = position (map (p2 *** vertex) ps)
            # connectList (map snd ps)

drawHg :: HyperGraph -> Diagram B
drawHg hg = mconcat (map drawPSet ps) <> mconcat (map drawBSet bs)
    where bs = map (map (verts hg !)) (bSets hg)
          ps = map (map (\ v -> (verts hg ! v, v))) (pSets hg)

hg1 :: HyperGraph
hg1 = HyperGraph
    { verts = fromList [ (p1, (1, 1))
                       , (p2, (1, 2))
                       , (q1, (3, 1))
                       , (q2, (3, 2))
                       ]
    , pSets = [[p1, p2], [q1, q2]]
    , bSets = [[p1, q1], [p2], [q2]]
    } where p1 = ("p", 1, 1)
            p2 = ("p", 2, 2)
            q1 = ("q", 1, 3)
            q2 = ("q", 2, 4)

hgEx2_1 :: HyperGraph
hgEx2_1 = HyperGraph
    { verts = fromList [ (q1, (1, 1))
                       , (p1, (2, 1))
                       , (p2, (3, 1))
                       , (g1, (4, 1))
                       , (f1, (3, 0))
                       ]
    , pSets = [[q1], [p1, p2], [f1], [g1]]
    , bSets = [[q1, p1], [f1, p2, g1]]
    }
    where q1 = ("q", 1, 1)
          p1 = ("p", 1, 2)
          p2 = ("p", 2, 3)
          g1 = ("g", 1, 4)
          f1 = ("f", 1, 5)

hgEx2_2 :: HyperGraph
hgEx2_2 = HyperGraph
    { verts = fromList [ (q1,   (1, 2))
                       , (p1_1, (2, 2))
                       , (p2_1, (3, 2))
                       , (p1_2, (1, 1))
                       , (p2_2, (1, 0))
                       , (g1,   (3, 1))
                       , (f1,   (2, 0))
                       ]
    , pSets = [[q1], [p1_1, p2_1], [p1_2, p2_2], [f1], [g1]]
    , bSets = [[p1_2, q1, p1_1], [p2_1, g1], [p2_2, f1]]
    }
    where q1   = ("q", 1, 1)
          p1_1 = ("p", 1, 2)
          p2_1 = ("p", 2, 3)
          p1_2 = ("p", 1, 4)
          p2_2 = ("p", 2, 5)
          g1   = ("g", 1, 6)
          f1   = ("f", 1, 7)

isomorphic :: HyperGraph
isomorphic = HyperGraph
    { verts = fromList [ (q1,   (1, 2))
                       , (p1_1, (2, 2))
                       , (p2_1, (3, 2))
                       , (p1_2, (1, 1))
                       , (p2_2, (1, 0))
                       ]
    , pSets = [[q1], [p1_1, p2_1], [p1_2, p2_2]]
    , bSets = [[p1_2, q1, p1_1], [p2_1], [p2_2]]
    }
    where q1   = ("q", 1, 1)
          p1_1 = ("p", 1, 2)
          p2_1 = ("p", 2, 3)
          p1_2 = ("p", 1, 4)
          p2_2 = ("p", 2, 5)

isomorphicReduced :: HyperGraph
isomorphicReduced = HyperGraph
    { verts = fromList [ (q1, (1, 2))
                       , (p1, (2, 2))
                       , (p2, (3, 2))
                       ]
    , pSets = [[q1], [p1, p2]]
    , bSets = [[q1, p1], [p2]]
    }
    where q1 = ("q", 1, 1)
          p1 = ("p", 1, 2)
          p2 = ("p", 2, 3)
