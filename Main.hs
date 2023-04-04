
import MazeBuilder ( buildMazeData )
import Solver ( solve )
import DataTypes ()
import IO (printSolutions )
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    text <- hGetContents file
    let maze = map (map read . words) $ lines text 
    let mazeData = buildMazeData maze
    let solutions = solve mazeData
    printSolutions maze solutions
