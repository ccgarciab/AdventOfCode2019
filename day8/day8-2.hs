import Data.List

chunksOf :: Int -> [a] -> [[a]]
chunksOf _   [] = []
chunksOf len xs = (take len xs):(chunksOf len (drop len xs))

pixel :: Char -> Char -> Char
pixel '2' p = p
pixel  p  _ = p

color :: Char -> Char
color '0' = ' '
color '1' = '#'

main = do
       input <- getLine
       mapM_ print $ chunksOf 25 $ map color $ foldl1 (zipWith pixel) $ chunksOf (25 * 6) input
