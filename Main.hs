
import MazeBuilder ( buildMazeData )
import Solver ( solve )
import DataTypes ()
import IO 
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    maze <- readMaze (head args)
    let solutions = solve (buildMazeData maze) in
     printSolutions maze solutions
