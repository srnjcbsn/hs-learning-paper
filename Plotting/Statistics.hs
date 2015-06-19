module Statistics where

import           Diagrams.Backend.PGF
import           Diagrams.Prelude

-- import           Control.Applicative           ((*>), (<*))
-- import           Control.Arrow                 (second, (***))
import           Control.Monad                 (void)
import           Data.List                     (group)
import           Data.Map                      (Map, fromList, fromListWith,
                                                mapWithKey, (!))
import qualified Data.Map                      as Map
import           System.FilePath               ()
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P

data Knl = Knl
    { posKn   :: Int
    , posUnkn :: Int
    , negKn   :: Int
    , negUnkn :: Int
    } deriving Show

type ActMap = Map String (Int, [ActKnl])

data ActKnl = ActKnl
    { effKnl :: Knl
    , preKnl :: Knl
    , cands  :: Int
    , sim    :: Int
    } deriving Show

r :: Double
r = 0.1

scf :: Int -> Double
scf n = 0.5 * (fromIntegral n)

yTrans :: Int -> Int -> Double
yTrans maxPreds n = (fromIntegral n / fromIntegral maxPreds) * scf maxPreds

mvPt maxPreds y col = moveTo (p2 (0.0, yTrans maxPreds y))
                             (circle r # fc col # lc col)

plotl mp vals = zipWith (\x y -> p2 (x, y))
                        [ fromIntegral x | x <- [1 ..] ]
                        (map (yTrans mp) vals)

posKnls :: Int -> [Knl] -> [[P2 Double]]
posKnls mp knls = [pks, pus, nks, nus] where
    pks = plotl mp (map posKn   knls)
    pus = plotl mp (map posUnkn knls)
    nks = plotl mp (map negKn   knls)
    nus = plotl mp (map negUnkn knls)

colors = [ ("positive proven", blue)
         , ("positive unproven", red)
         , ("negative proven", green)
         , ("negative unproven", yellow)
         , ("candidates", black)
         ]

drawEk :: Int -> [Knl] -> [Int] -> Diagram B
drawEk mp knls steps = (frame <> plot) === strutY 1.0 === legend where
    -- maxC = yTrans mp $ maximum cs
    fA = yTrans mp mp
    maxHeight = fA
    width = fromIntegral $ length knls
    botLeft = p2 (0.0, 0.0)
    frame =  botLeft ~~ (p2 (width, 0.0))
          <> botLeft ~~ (p2 (0.0, maxHeight))
          <> (p2 (0.0, fA)) ~~ (p2 ((-0.2), fA))
          <> moveTo (p2 ((-2.0), fA)) (baselineText "$\\mathbb{F}_A$" # translateY (-0.3))
          <> mconcat (map tick [ 5, 10 .. length knls])
          <> mconcat (map stepTick stepchanges)
    stepTick n = p2 (fromIntegral n, 0.0) ~~ p2 (fromIntegral n, (-1.0))
               # lc red
    stepchanges = map length $ group steps
    tick n = p2 (fromIntegral n, 0.0) ~~ p2 (fromIntegral n, (-0.5))
    plot = mconcat
         $ zipWith (\vs col -> fromVertices vs # lc col)
                   (posKnls mp knls)
                   (map snd colors)
    legend = vsep 0.5 $ map (uncurry aLegend) colors
    aLegend d c =   circle 0.4 # fc c # lc c
                ||| strutX 1.0
                ||| baselineText d # translateY (-0.3)

drawPk :: Int -> [Knl] -> [Int] -> [Int] -> Diagram B
drawPk mp knls cs steps = (frame <> plot) === strutY 1.0 === legend where
    maxC = yTrans mp $ maximum cs
    fA = yTrans mp mp
    maxHeight = max maxC fA
    width = fromIntegral $ length knls
    botLeft = p2 (0.0, 0.0)
    frame =  botLeft ~~ (p2 (width, 0.0))
          <> botLeft ~~ (p2 (0.0, maxHeight))
          <> (p2 (0.0, fA)) ~~ (p2 ((-0.2), fA))
          <> moveTo (p2 ((-2.0), fA)) (baselineText "$\\mathbb{F}_A$" # translateY (-0.3))
          <> mconcat (map tick [ 5, 10 .. length knls])
          <> mconcat (map stepTick stepchanges)
    stepTick n = p2 (fromIntegral n, 0.0) ~~ p2 (fromIntegral n, (-0.5))
               # lc red
    stepchanges = map length $ group steps
    tick n = p2 (fromIntegral n, 0.0) ~~ p2 (fromIntegral n, (-0.5))
    plot = mconcat
         $ zipWith (\vs col -> fromVertices vs # lc col)
                   (posKnls mp knls ++ [plotl mp cs])
                   (map snd colors)
    legend = vsep 0.5 $ map (uncurry aLegend) colors
    aLegend d c =   circle 0.4 # fc c # lc c
                ||| strutX 1.0
                ||| baselineText d # translateY (-0.3)


drawAct :: String -> String -> (Int, [ActKnl]) -> [(String, Diagram B)]
drawAct p n (mp, aks) = [(pkName, pkd), (ekName, ekd)] where
    pkName = p ++ "-pk-" ++ n
    ekName = p ++ "-ek-" ++ n
    pkd = drawPk mp (map preKnl aks) (map cands aks) (map sim aks)
    ekd = drawEk mp (map effKnl aks) (map sim aks)
    -- (,) n
    -- (hcat $ map (\ak -> drawPk mp (preKnl ak) (cands ak)) aks)

drawHist :: FilePath -> IO [(String, Diagram B)]
drawHist path = do
    histE <- parseHistFile path
    let hist = case histE of
                   Right am -> am
                   Left err -> error $ show err
    return $ concatMap (uncurry (drawAct path)) $ Map.toList hist

integer :: Parser Int
integer = read <$> many1 digit

parseName :: Parser String
parseName = manyTill (P.noneOf ":\n") (char ':') <* space

parsePredSize :: Parser (String, Int)
parsePredSize = (,) <$> parseName <*> integer <* newline

parseStep :: Parser ()
parseStep = string "Step " >> integer >> newline >> return ()

parseKnl :: Parser Knl
parseKnl = Knl <$> num <*> num <*> num <*> num
    where num = parseNumComma

parseNumComma :: Parser Int
parseNumComma = integer <* (char ',' >> space)

parseActMapping :: Parser (String, ActKnl)
parseActMapping = do
    name <- parseName
    simn <- parseNumComma
    eK   <- parseKnl
    pK   <- parseKnl
    c    <- integer <* newline
    return (name, ActKnl eK pK c simn)

parseSimStep :: Parser [(String, ActKnl)]
parseSimStep = manyTill parseActMapping parseStep

parseBreak :: Parser ()
parseBreak = void $ string "-- RUNNING --\n"

parseSim :: Parser (Map String (Int, [ActKnl]))
parseSim = do
    predS <- manyTill parsePredSize parseBreak
    steps <- many1 parseActMapping
    let acts = fromListWith (flip (++)) [ (n, [ak]) | (n, ak) <- steps]
        sizes = fromList predS
        finalMap = mapWithKey (\k a -> (sizes ! k, a)) acts
    -- steps <- sepEndBy1 (many (try parseActMapping)) parseStep
    -- let acts :: Map String [ActKnl]
    --     acts = fromListWith (++) [ (n, [ak]) | (n, ak) <- concat steps]
    --     sizes :: Map String Int
    --     sizes = fromList predS
    --     finalMap = mapWithKey (\k a -> (sizes ! k, a)) acts
    return finalMap

parseHistFile :: FilePath -> IO (Either ParseError ActMap)
parseHistFile = parseFromFile parseSim
