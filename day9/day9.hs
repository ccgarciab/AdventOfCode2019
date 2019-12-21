import qualified Data.Map.Strict as M

type Program = M.Map Integer Integer

data Computer = Computer { program :: Program
                         , input   :: [Integer]
                         , output  :: [Integer]
                         , iptr    :: Integer
                         , bptr    :: Integer
                         } deriving (Show, Read)

data Mode = Position | Inmediate | Relative deriving (Show, Eq)


splitBy :: (Foldable t, Eq p) => p -> t p -> [[p]]
splitBy delimiter = foldr f [[]] 
  where
    f c l@(x:xs) | c == delimiter = []:l
                 | otherwise = (c:x):xs

cLookup :: Integer -> Computer -> Maybe Integer
cLookup pos c = if pos < 0 then Nothing else case M.lookup pos (program c) of
                                                                 Nothing -> Just 0
                                                                 just    -> just

popInput :: Computer -> Maybe (Computer, Integer)
popInput c = do
    n      <- if null $ input c then Nothing else Just $ head $ input c
    pInput <- if null $ input c then Nothing else Just $ tail $ input c
    return (c{input= pInput}, n)    

pushOut :: Computer -> Integer -> Computer
pushOut c out = c{output= out:(output c)}

insertInProg :: Computer -> Integer -> Integer -> Computer
insertInProg c elem pos = c{program= M.insert pos elem (program c)}

takeFromProg :: Computer -> Integer -> Maybe [Integer]
takeFromProg c n = sequence $ map (\i -> cLookup (i + (iptr c)) c) [1..n]

parseOp :: Integer -> (Integer, [Mode], Integer)
parseOp n = (op, modes op, fromIntegral arity)
  where
    op     = n `mod` 100
    arity  = maybe undefined id $ lookup op $ [(i, 1) | i<-[3, 4, 9]] ++ 
                                              [(i, 2) | i<-[5, 6]] ++ 
                                              [(i, 3) | i<-[1, 2, 7, 8]] ++ 
                                              [(99, 0)]
    ms  = map f $ drop 2 $ reverse $ show n
    len = arity - (length ms)
    f x
        | x == '0' = Position
        | x == '1' = Inmediate
        | x == '2' = Relative
    modes op'
        | op' `elem` [4, 9]  = take 1 $ ms ++ [Position]
        | op' == 3           = take 1 $ ms ++ [Inmediate]
        | op' == 99          = []
        | op' `elem` [1..8]  = (take 2 $ ms ++ [Position, Position]) ++ weirdTail
    weirdTail = if (length ms) == 3 then [last ms] else [Inmediate]

runOp :: Computer -> Integer -> [Integer] -> [Mode] -> Maybe Computer
runOp c op args modes
    | op `elem` [5, 6] = do
                         (b:p:[]) <- values args
                         return $ if (b == 0) == (op == 5) then c else c{iptr=p - 3}
    | op == 4          = do
                         [val] <- values args
                         return $ pushOut c val
    | op == 3          = do
                         (new_c, val) <- popInput c
                         let pos = position $ head args
                         return $ insertInProg new_c val pos
    | op `elem` [1..8] = do
                         (a:b:[]) <- values $ take 2 args
                         let pos = position $ last args
                         let toCBool bf x y = if x `bf` y  then 1 else 0
                         f <- lookup op [(1, (+)), (2, (*)), (7, toCBool (<)), (8, toCBool (==))]
                         return $ insertInProg c (a `f` b) pos
    | op == 9          = do
                         [val] <- values args
                         return $ c{bptr= bptr c + val}
    | op == 99         = return c{iptr= -1}
    | otherwise        = Nothing
  where
    select mode arg
        | mode == Position = cLookup arg c
        | mode == Relative = cLookup (bptr c + arg) c
        | otherwise        = Just arg
    values as = sequence $ zipWith select modes as
    position  = (+) (if last modes == Relative then bptr c else 0)

run :: Computer -> Maybe Computer
run c = do
    rawOp   <- cLookup (iptr c) c
    let (op, modes, arity) = parseOp rawOp
    args    <- takeFromProg c arity
    nextC   <- runOp c op args modes
    case op of
        99 -> Just c
        _  -> run $ nextC{iptr= arity + 1 + (iptr nextC)}

main = do
    rawInput <- getLine
    let inp = map read $ splitBy ',' rawInput
    let prog  = M.fromList $ zip [0..] inp
    let comp  = Computer{program=prog, input=[2], output=[], iptr=0, bptr=0}
    print $ run comp
