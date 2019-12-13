import qualified Data.Map.Strict as M
import Data.Maybe

type Program = M.Map Integer Integer

init_base :: [Integer] -> Program
init_base = M.fromList . zip [0..]

init_args :: (Integer, Integer) -> Program -> Program
init_args (n, v) = M.insert 2 v . M.insert 1 n

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

extract_ans :: Program -> (Integer, Integer) -> Maybe (Integer, Integer)
extract_ans program (n, v) = do
    resultProg <- run 0 $ init_args (n, v) program
    resultNum  <- M.lookup 0 resultProg
    return (resultNum, 100 * n + v)

main :: IO ()
main = do
    baseInput <- getLine
    let base = init_base $ map read $ splitBy ',' baseInput
    let nvs = [(n, v) | n <- [0..99], v <- [0..99]]
    print $ snd $ head $filter ((==19690720) . fst) $ catMaybes $ map (extract_ans base) nvs
