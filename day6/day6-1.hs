import qualified Data.Map.Strict as M

-- N-ary Tree
data NT a = NT a [NT a] deriving (Show)

buildTree :: Ord a => M.Map a [a] -> a -> NT a
buildTree m k = NT k $ map (buildTree m) $ maybe [] id $ M.lookup k m

levelsTree :: Num t => NT a -> NT t
levelsTree t = let f n (NT _ l) = NT n $ map (f (n + 1)) l in f 0 t

sumTree :: Num a => NT a -> a
sumTree (NT v l) = v + (sum $ map sumTree l)

insertCons :: Ord k => M.Map k [a] -> (k, a) -> M.Map k [a]
insertCons m (k, v) = case M.lookup k m of
    Nothing -> M.insert k [v] m
    Just l  -> M.insert k (v:l) m

splitWithDelim :: Eq a => a -> [a] -> ([a], [a])
splitWithDelim e l = (takeWhile (/= e) l, drop 1 $ dropWhile (/= e) l)

main = interact $ show . sumTree . levelsTree . (\m -> buildTree m "COM") . foldl insertCons M.empty . map (splitWithDelim ')') . lines
