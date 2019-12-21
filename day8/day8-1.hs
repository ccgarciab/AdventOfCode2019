import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _   [] = []
chunksOf len xs = (take len xs):(chunksOf len (drop len xs))

main = interact $ show . (\l -> (count '1' l) * (count '2' l)) . head . sortOn (count '0') . chunksOf (25 * 6) . head . lines
