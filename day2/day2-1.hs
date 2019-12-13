import qualified Data.Map.Strict as M

type Program = M.Map Integer Integer

init_program :: [Integer] -> Program
init_program = M.insert 2 2 . M.insert 1 12 . M.fromList . zip [0..]

splitBy :: (Foldable t, Eq p) => p -> t p -> [[p]]
splitBy delimiter = foldr f [[]] 
  where
    f c l@(x:xs) | c == delimiter = []:l
                 | otherwise = (c:x):xs

run :: Integer -> Program -> Maybe Program
run pointer prog = do
    opcode <- M.lookup pointer prog
    apoint <- M.lookup (pointer + 1) prog
    aval   <- M.lookup apoint prog
    bpoint <- M.lookup (pointer + 2) prog
    bval   <- M.lookup bpoint prog
    pos    <- M.lookup (pointer + 3) prog
    case opcode of
        99 -> Just prog
        1  -> run (pointer + 4) $ M.insert pos (aval + bval) prog
        2  -> run (pointer + 4) $ M.insert pos (aval * bval) prog
        _  -> Nothing

main :: IO ()
main = interact $ (++"\n") . maybe "Error" show . maybe Nothing id . 
       fmap (M.lookup 0) . run 0 . init_program . map read . splitBy ','
