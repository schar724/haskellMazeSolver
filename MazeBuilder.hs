module MazeBuilder (buildMazeData, getAdjMatrix, getStart, getEnd) where
import Data.List (elemIndex, findIndex)
import DataTypes 


--Returns a list of column numbers associated with traversalbe nodes (1's)
getColumn :: [Int] -> Int -> [Int] -> [Int]
getColumn [] _ cols = cols
getColumn (x:xs) num cols = if x == 1 || x == 8 || x == 9 then checkRest (num:cols)
                            else checkRest cols
    where checkRest = getColumn xs (num+1)

--Returns a list of pairs, the first element of the pair is a row number and the second is a list of
--the columns in that row that have traversable nodes.
getCords :: Maze -> Int -> [(Int,[Int])] -> [(Int, [Int])]
getCords [] _ cords = cords
getCords (x:xs) num cords = let columns = getColumn x 0 [] in
                            if null columns then checkRest cords
                            else checkRest ((num, columns):cords)
    where checkRest = getCords xs (num+1)

--converts the list of pairs to a list of coordinate
getCordsList :: [(Int,[Int])] -> [Coord]
getCordsList [] = []
getCordsList (x:xs) = getCordsList xs ++ unpackCords x

--helper functtion for GetCordsList
unpackCords :: (Int,[Int]) -> [Coord]
unpackCords (row,[]) = []
unpackCords (row,col:cols) = (row,col): unpackCords (row,cols)

--From a list of coordinates, returns an adjacency matrix of adjacent nodes
buildAdjMatrix :: [Coord] -> AdjMatrix
buildAdjMatrix coords = let adjacent c = filter (\x -> x /= c && isAdjacent c x) coords in [(c, adjacent c) | c <- coords]

isAdjacent :: Coord -> Coord -> Bool
isAdjacent (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2) == 1

--Returns the coordinates of the start(8) and end(9) nodes of the maze
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