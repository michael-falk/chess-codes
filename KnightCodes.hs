module KnightCodes where 

makeMove :: (a -> [a]) -> [a] -> [[a]]
makeMove f xs = (:) <$> (f (head xs)) <*> [xs]

type Piece = (Int,Int)

inBoard :: Piece -> Bool
inBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

moveKnight :: Piece -> [Piece]  
moveKnight (c,r) =
    let candidates = [(c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1),
                      (c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2)]
    in filter inBoard candidates

moveBishop :: Piece -> [Piece]
moveBishop (c,r) = 
    let range = [1..8]
        diagonals i = [(c+i,r+i), (c+i,r-i), (c-i,r+i), (c-i,r-i)]
        candidates = range >>= diagonals
    in filter inBoard candidates

in3 :: Piece -> [[Piece]]
in3 start = 
    let move = (makeMove moveKnight)
    in  [[start]] >>= move >>= move >>= move

reachIn :: (Piece -> [Piece]) -> Piece -> Int -> Maybe [[Piece]]
reachIn move start n = 
    let lists = take (n + 1) $ iterate (>>= makeMove move) [[start]]
    in  case lists of []    -> Nothing
                      lists -> Just (last lists)
