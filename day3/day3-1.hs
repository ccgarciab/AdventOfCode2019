import qualified Data.Set as S
import Data.List

data Direction = U | D | R | L deriving (Read, Show)

type Coord = (Integer, Integer)

type Move = (Direction, Integer)

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
    
mhDist :: Coord -> Integer
mhDist (a, b) = (abs a) + (abs b)

main :: IO ()    
main = do
    [line1, line2] <- sequence [getLine, getLine]
    let [path1, path2] = map (S.fromList . (traceWire (0, 0)) . parsePath) [line1, line2]
    print $ head . sort $ map mhDist $ S.toList $ path1 `S.intersection` path2
