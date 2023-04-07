import Test.HUnit
import DataTypes
import MazeBuilder (buildMazeData)
import Solver (solve)
import IO (printSolutions )
import System.IO

main :: IO ()
main = do
    runTestTT tests
    return ()

readMaze :: String -> IO Maze
readMaze fileName = do
    contents <- readFile fileName
    let maze = map (map read . words) $ lines contents
    return maze

-- Test cases
testCases :: [(String, String, Bool)]
testCases = [("Test adjacent_s_and_e", "./tests/adjacent_s_and_e.txt", True), 
            ("Test all_paths_no_s_or_e", "./tests/all_paths_no_s_or_e.txt", False),
            ("Test all_walls_no_s_or_e", "./tests/all_walls_no_s_or_e.txt", False),
            ("Test empty_file", "./tests/empty_file.txt", False),
            ("Test illegal_char", "./tests/illegal_char.txt", False),
            ("Test imperfect_maze", "./tests/imperfect_maze.txt", True),
            ("Test multiple exits", "./tests/multiple_exits.txt", True),
            ("Test no exit", "./tests/no_exit.txt", False),
            ("Test one cycle rest walls", "./tests/one_cycle_rest_walls.txt", True),
            ("Test perfect maze", "./tests/perfect_maze.txt", True),
            ("Test Rectangle no path", "./tests/rectangle_no_path.txt", False),
            ("Test simple path rest walls", "./tests/simple_path_rest_walls.txt", True),
            ("Test smaller dims", "./tests/smaller_dims.txt", True)
            ]

-- Helper function that returns true if result is not [[(-1,-1)]]
isSolved :: [[Coord]] -> Bool
isSolved result = result /= [[(-1,-1)]]

testMazeSolver :: (String, String, Bool) -> Test
testMazeSolver (testName, fileName, expected) = TestLabel testName $ TestCase $ do
    maze <- readMaze fileName
    if maze == [] then do
        putStrLn $ "\nTest: " ++ testName ++ "\n"
        assertEqual testName expected False
    else if (length $ filter (\x -> x `notElem` [0,1,8,9]) $ concat maze) > 0 then do
            putStrLn $ "\nTest: " ++ testName ++ "\n"
            assertEqual testName expected False
    else do
        let result = solve $ buildMazeData maze
        putStrLn $ "\nTest: " ++ testName ++ "\nResult: "
        printSolutions maze result
        assertEqual testName expected (isSolved result)

tests :: Test
tests = TestList $ map testMazeSolver testCases
