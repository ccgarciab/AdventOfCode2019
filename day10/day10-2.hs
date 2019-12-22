import Data.Function
import Data.List
import Data.Maybe

type Coord = (Int, Int)

asteroidCoords :: String -> [Coord]
asteroidCoords s = concat $ [tagCols r row | (r, row)<-taggedRows]
  where
    taggedRows = zip [0..] $ lines s
    tagCols r' row' = catMaybes $ zipWith (tagCol r') [0..] row'
    tagCol y x c = if c == '#' then Just (x, y) else Nothing

mhNorm :: Coord -> Int
mhNorm (x, y) = ((+) `on` abs) x y

vecFrom :: Coord -> Coord -> Coord
vecFrom (x, y) (a, b) = (a - x, b - y)

coordFrom :: Coord -> Coord -> Coord
coordFrom (x, y) (a, b) = (a + x, b + y)

angle :: (RealFloat a) => Coord -> a
angle (x, y) = (atan2 x' y') + (if x < 0 then 2 * pi else 0)
  where
    (x', y') = (fromIntegral x, fromIntegral (-y))

hides :: Coord -> Coord -> Bool
c1 `hides` c2 = (unit c1) == (unit c2)
  where
    unit (x, y) = let d = x `gcd` y in (x `div` d, y `div` d)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ( (==) `on` f)

globalGroupOn :: Ord b => (a -> b) -> [a] -> [[a]]
globalGroupOn f = groupOn f . sortOn f

colectVisibles :: [Coord] -> [Coord] -> [Coord]
colectVisibles visibles testing = visibles ++ newVis
  where
    newVis = do
             t <- testing
             case any (`hides` t) visibles of
                 True  -> []
                 False -> return t
                 
partitionVisibles :: ([Coord], [Coord]) -> [Coord] -> ([Coord], [Coord])
partitionVisibles (visibles, blocked) testing = (visibles ++ newVis, blocked ++ newBlock)
  where (newBlock, newVis) = partition (\crd -> any (`hides` crd) visibles) testing

numVisibles :: [Coord] -> [(Coord, Int)]
numVisibles xs = do
                 x <- xs
                 let orderedFromX = tail $ globalGroupOn mhNorm $ map (vecFrom x) xs
                 let nVisible = length $ foldl1 colectVisibles orderedFromX
                 return (x, nVisible)

findBest :: [Coord] -> Coord
findBest = fst . head . reverse . sortOn snd . numVisibles

vaporizeList :: [Coord] -> [Coord]
vaporizeList []   = []
vaporizeList vecs = sortOn angle visibles ++ vaporizeList blocked
  where
    (visibles, blocked) = foldl partitionVisibles ([], []) orderedFromStation
    orderedFromStation  = globalGroupOn mhNorm vecs

main = do
       asteroids <- fmap asteroidCoords getContents
       let station = findBest asteroids
       let vecs    = delete (0, 0) $ map (vecFrom station) asteroids
       print $ map (coordFrom station) $ take 1 $ drop 199 $ vaporizeList vecs
