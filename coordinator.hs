import Data.Bits

getColumn :: [Int] -> Int -> [Int] -> [Int]
getColumn [] _ cols = cols
getColumn (x:xs) num cols = if x == 1  then checkRest (num:cols)
                            else checkRest cols
    where checkRest = getColumn xs (num+1)

getCords :: [[Int]] -> Int -> [(Int,[Int])] -> [(Int, [Int])]
getCords [] _ cords = cords
getCords (x:xs) num cords = let columns = getColumn x 0 [] in
                            if null columns then checkRest cords
                            else checkRest ((num, columns):cords)  
    where checkRest = getCords xs (num+1)

getCordsList :: [(Int,[Int])] -> [(Int,Int)]
getCordsList [] = []
getCordsList (x:xs) = unpackCords x ++ getCordsList xs

unpackCords :: (Int,[Int]) -> [(Int,Int)]
unpackCords (row,[]) = []
unpackCords (row,col:cols) = (row,col): unpackCords (row,cols)

getAdjMatrix :: [(Int,Int)] -> [((Int,Int),[(Int,Int)])]
getAdjMatrix [] = []
getAdjMatrix (x:xs) = (x, getEdges x xs []) : getAdjMatrix xs 

getEdges :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
getEdges _ [] edges = edges
getEdges (row,col) (x:xs) edges =   if xor (abs(row - fst x) == 1) (abs(col - snd x) ==1) 
                                    then getEdges (row,col) xs (x:edges)
                                    else getEdges (row,col) xs edges