
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
    if (length maze == 0) || (length (head maze) == 0) then
     putStr "File is empty\n"
    else if (length $ filter (\x -> x `notElem` [0,1,8,9]) $ concat maze) > 0 then
     putStr "File contains invalid characters\n" 
    else 
     let solutions = solve (buildMazeData maze) in
     printSolutions maze solutions
