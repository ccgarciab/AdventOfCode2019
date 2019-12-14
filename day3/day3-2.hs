import qualified Data.Map.Strict as M
import Data.List

data Direction = U | D | R | L deriving (Read, Show)

type Coord = (Integer, Integer)

type Move = (Direction, Integer)

type Path = M.Map Coord Integer

splitBy :: (Foldable t, Eq p) => p -> t p -> [[p]]
splitBy delimiter = foldr f [[]] 
  where
    f c l@(x:xs) | c == delimiter = []:l
                 | otherwise = (c:x):xs

parseMove :: String -> Move
parseMove (d:n) = (read [d], read n)

parsePath :: String -> [Move]
parsePath = map parseMove . splitBy ','

occupyCells :: Coord -> Move -> [Coord]
occupyCells (x, y) (dir, len) = map f [1..len]
  where
    f = case dir of
        U -> (\i -> (x, y + i))
        D -> (\i -> (x, y - i))
        R -> (\i -> (x + i, y))
        L -> (\i -> (x - i, y))

traceWire :: Coord -> [Move] -> [Coord]
traceWire _      []     = [] 
traceWire origin (m:ms) = cells ++ (traceWire (last cells) ms)
  where
    cells = occupyCells origin m

insertOnce :: Path -> (Coord, Integer) -> Path
insertOnce record (k, v) = M.insertWith (flip const) k v record

main :: IO ()    
main = do
    [line1, line2] <- sequence [getLine, getLine]
    let string2CoordDistance = (flip zip $ [1..]) . (traceWire (0, 0)) . parsePath
    -- coordDist :: [(Coord, Integer)]
    let [coordDist1, coordDist2] = map string2CoordDistance [line1, line2]
    let [path1, path2]   = map (foldl insertOnce M.empty) [coordDist1, coordDist2]
    print $ head $ sort $ map snd $ M.toList $ M.intersectionWith (+) path1 path2

