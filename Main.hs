
import MazeBuilder
import Solver
import DataTypes
import IO

main :: String -> IO ()
main path =
    do p <- readPath path
    case solve (buildMazeData p) of
        [] -> putStr "no solution"
        solution -> printSolution solution 