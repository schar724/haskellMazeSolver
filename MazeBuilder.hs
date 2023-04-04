module MazeBuilder (buildMazeData, getAdjMatrix, getStart, getEnd) where
    
import Data.List (elemIndex, findIndex)
import DataTypes 

getColumn :: [Int] -> Int -> [Int] -> [Int]
getColumn [] _ cols = cols
getColumn (x:xs) num cols = if x == 1 || x == 8 || x == 9 then checkRest (num:cols)
                            else checkRest cols
    where checkRest = getColumn xs (num+1)

getCords :: Maze -> Int -> [(Int,[Int])] -> [(Int, [Int])]
getCords [] _ cords = cords
getCords (x:xs) num cords = let columns = getColumn x 0 [] in
                            if null columns then checkRest cords
                            else checkRest ((num, columns):cords)
    where checkRest = getCords xs (num+1)

getCordsList :: [(Int,[Int])] -> [Coord]
getCordsList [] = []
getCordsList (x:xs) = getCordsList xs ++ unpackCords x

unpackCords :: (Int,[Int]) -> [Coord]
unpackCords (row,[]) = []
unpackCords (row,col:cols) = (row,col): unpackCords (row,cols)

buildAdjMatrix :: [Coord] -> AdjMatrix
buildAdjMatrix coords = let adjacent c = filter (\x -> x /= c && isAdjacent c x) coords in [(c, adjacent c) | c <- coords]

isAdjacent :: Coord -> Coord -> Bool
isAdjacent (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2) == 1

getStartAndEnd :: Maze -> Maybe (Coord,Coord)
getStartAndEnd xs = do
    startRow <- findIndex (elem 8) xs
    endRow <- findIndex (elem 9) xs
    let maybeStartCol = elemIndex 8 (xs !! startRow)
    let maybeEndCol = elemIndex 9 (xs !! endRow)
    case (maybeStartCol, maybeEndCol) of
        (Just startCol, Just endCol) -> return ((startRow, startCol), (endRow,endCol))
        _  -> Nothing

buildMazeData:: Maze -> MazeData
buildMazeData list = let startAndEnd = getStartAndEnd list in 
    Package (buildAdjMatrix (getCordsList (getCords list 0 []))) startAndEnd

getStart :: MazeData -> Coord
getStart (Package _ Nothing) = (-1,-1)
getStart (Package _ (Just (start, end))) = start

getEnd:: MazeData -> Coord
getEnd (Package _ Nothing) = (-1,-1)
getEnd (Package _ (Just (start, end))) = end

getAdjMatrix :: MazeData -> AdjMatrix
getAdjMatrix (Package adjMatrix _) = adjMatrix