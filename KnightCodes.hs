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

moveRook :: Piece -> [Piece]
moveRook (c,r) = 
    let range = [1..8]
        straights i = [(c+i,r), (c,r-i), (c-i,r), (c,r+i)]
        candidates = range >>= straights
    in filter inBoard candidates

moveQueen :: Piece -> [Piece]  
moveQueen (c,r) =
    let range = [1..8]
        star i = [(c+i,r), (c,r-i), (c-i,r), (c,r+i),
                (c+i,r+i), (c+i,r-i), (c-i,r+i), (c-i,r-i)]
        candidates = range >>= star
    in filter inBoard candidates

pathsIn :: (Piece -> [Piece]) -> Piece -> Int -> Maybe [[Piece]]
pathsIn move start n = 
    let lists = take (n + 1) $ iterate (>>= makeMove move) [[start]]
    in  case lists of []    -> Nothing
                      lists -> Just (last lists)
