module Statistics where

import           Diagrams.Backend.PGF
import           Diagrams.Prelude

-- import           Control.Applicative           ((*>), (<*))
-- import           Control.Arrow                 (second, (***))
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
    } deriving Show

r :: Double
r = 0.1

scf :: Double
scf = 5.0

drawKnl :: Int -> Knl -> Diagram B
drawKnl maxPreds knl = pk <> pu <> nk <> nu where
    pu = moveTo (p2 (0.0, yTrans (posUnkn knl))) (circle r # fc red # lc red)
    pk = moveTo (p2 (0.0, yTrans (posKn   knl))) (circle r # fc green # lc green)
    nu = moveTo (p2 (0.0, yTrans (negUnkn knl))) (circle r # fc blue # lc blue)
    nk = moveTo (p2 (0.0, yTrans (negKn   knl))) (circle r # fc black # lc black)
    yTrans n = (fromIntegral n / fromIntegral maxPreds) * scf

drawAct :: String -> (Int, [ActKnl]) -> (String, Diagram B)
drawAct n (maxPreds, aks) = (,) n
    (hcat $ map (drawKnl maxPreds . effKnl) aks)

drawHist :: FilePath -> IO [(String, Diagram B)]
drawHist path = do
    histE <- parseHistFile path
    let hist = case histE of
                   Right am -> am
                   Left err -> error $ show err
    print (Map.keys hist)
    return $ map (uncurry drawAct) $ Map.toList hist

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
    where num = integer <* (char ',' >> space)

parseActMapping :: Parser (String, ActKnl)
parseActMapping = do
    name <- parseName
    eK   <- parseKnl
    pK   <- parseKnl
    c    <- integer <* newline
    return (name, ActKnl eK pK c)

parseSimStep :: Parser [(String, ActKnl)]
parseSimStep = manyTill parseActMapping parseStep

parseSim :: Parser (Map String (Int, [ActKnl]))
parseSim = do
    predS <- manyTill parsePredSize parseStep
    steps <- sepEndBy1 (many (try parseActMapping)) parseStep
    let acts :: Map String [ActKnl]
        acts = fromListWith (++) [ (n, [ak]) | (n, ak) <- concat steps]
        sizes :: Map String Int
        sizes = fromList predS
        finalMap = mapWithKey (\k a -> (sizes ! k, a)) acts
    return finalMap

parseHistFile :: FilePath -> IO (Either ParseError ActMap)
parseHistFile = parseFromFile parseSim
