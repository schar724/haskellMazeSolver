module DataTypes (Coord, AdjMatrix, Maze, MazeData(..)) where

type Maze = [[Int]]
type Coord = (Int,Int)
type AdjMatrix = [(Coord, [Coord])]
data MazeData = Package AdjMatrix (Maybe (Coord,Coord))
    deriving (Show)