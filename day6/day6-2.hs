import qualified Data.Map as M

splitWithDelim :: Eq a => a -> [a] -> ([a], [a])
splitWithDelim e l = (takeWhile (/= e) l, drop 1 $ dropWhile (/= e) l)

flipInsert :: Ord k => M.Map k a -> (a, k) -> M.Map k a
flipInsert m (v, k) = M.insert k v m

trimDif :: Eq b => [b] -> [b] -> ([b], [b])
trimDif l1@(x:xs) l2@(y:ys)
    | x == y     = trimDif xs ys
    | otherwise  = (l1, l2)
trimDif l1 l2    = (l1, l2)

findPath :: Ord a => a -> M.Map a a -> [a]
findPath k m = reverse $ f k m
  where
    f k' m' = case M.lookup k' m' of
                    Nothing -> []
                    Just v' -> v':(f v' m')

main = do
     mp <- fmap (foldl flipInsert M.empty . map (splitWithDelim ')') . lines) getContents
     let (youPath, sanPath) = trimDif (findPath "YOU" mp) (findPath "SAN" mp)
     print $ subtract 2 $ sum $ map length [youPath, sanPath]
