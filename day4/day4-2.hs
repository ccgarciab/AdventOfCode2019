import Data.List

input :: [String]
input = map show [245318..765747]

ascending :: Ord a => [a] -> Bool
ascending l = and $ zipWith (<=) l (tail l)

hasPair :: Eq a => [a] -> Bool
hasPair = not . null . filter (==2) . map length . group

main = print $ length $ filter (\l -> (ascending l) && (hasPair l)) input
