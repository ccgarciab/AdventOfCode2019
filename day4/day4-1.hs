import Data.List

input :: [String]
input = map show [245318..765747]

ascending :: Ord a => [a] -> Bool
ascending l = and $ zipWith (<=) l (tail l)

repeats :: Eq a => [a] -> Bool
repeats = not . null . filter (\l -> (length l) > 1) . group

main = print $ length $ filter (\l -> (ascending l) && (repeats l)) input
