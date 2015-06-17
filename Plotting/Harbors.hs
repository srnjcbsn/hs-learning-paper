module Harbors where

import           Diagrams.Backend.PGF
import           Diagrams.Prelude

shaft1 = arc xDir (1/10 @@ turn)
shaft2 = arc xDir (1/3 @@ turn)

opts1 = with & arrowShaft .~ shaft1
             & arrowHead .~ tri & headLength .~ large
             & shaftStyle %~ lw veryThick . lc black

opts2 = with & arrowShaft .~ shaft2
             & shaftStyle %~ lw medium . lc black . dashingN [0.01, 0.01] 0

conn2 n m = connectPerim' opts2 n m na ma where
            na = ((fromIntegral n) / 7 @@ turn)
            ma = ((fromIntegral m - 2) / 7 @@ turn)

conn1 n m = connectPerim' opts1 n m na ma where
            na = ((fromIntegral n + 1) / 7 @@ turn)
            ma = ((fromIntegral m - 3) / 7 @@ turn)


c (n, a) =  circle 0.15 # named n
         <> text [a] # fontSize (local 0.1) # italic
dirs     = iterate (rotateBy (1/7)) unitX
cdirs    = zip dirs (map c $ zip [1 .. 7 :: Int] ['A' .. 'Z'])

example1 :: Diagram B
example1 = appends (circle 1) cdirs
         # conn1 (1 :: Int) (2 :: Int)
         # conn1 (2 :: Int) (3 :: Int)
         # conn1 (3 :: Int) (4 :: Int)
         # conn1 (4 :: Int) (5 :: Int)
         # conn1 (5 :: Int) (6 :: Int)
         # conn1 (6 :: Int) (7 :: Int)
         # conn2 (1 :: Int) (2 :: Int)
