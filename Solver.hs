module Solver (solve) where
import DataTypes 
import MazeBuilder

solve :: MazeData -> [[Coord]]
solve mazeData = do
    let adjMatrix = getAdjMatrix mazeData
    let start = getStart mazeData
    let end = getEnd mazeData 
    findPaths adjMatrix start end


--Depth first search
--Adapted from https://gist.github.com/abhin4v/9fc8eff25c6b1a14b60e
--and https://stackoverflow.com/questions/12739028/dfs-implementation-in-haskell
findPaths :: AdjMatrix -> Coord -> Coord -> [[Coord]]
findPaths adjMatrix start end = dfs [] [start]
    where 
        dfs visited [] = []
        dfs visited (current:rest)
            | current == end = [reverse visited ++ [current]]
            | isVisited current = []
            | otherwise = concat [dfs (current:visited) (p:rest) | p <- neighbors]
                where 
                    paths = map fst adjMatrix
                    neighbors = [n | (p, ns) <- adjMatrix, p == current, n <- ns]
                    isVisited coord = coord `elem` visited
