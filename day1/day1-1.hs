calcFuel :: Integer -> Integer
calcFuel n = (n `div` 3) - 2

main :: IO ()
main = interact $ (++"\n") . show . sum . map calcFuel . map read . lines
