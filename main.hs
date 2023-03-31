type Maze = [[Int]]

-- main :: String -> IO ()
-- main path = 
--     do p <- readMaze path
--         case solve p of
--             [] -> putStr "No Solution"
--             (sol:_) -> printSol




readMaze :: FilePath -> IO Maze
readMaze filePath = do
  contents <- readFile filePath
  let maze = map (map read . words) $ lines contents
  if (length maze == 0) || (length (head maze) == 0) then
    error "File is empty"
  else if (length $ filter (\x -> x `notElem` [0,1,8,9]) $ concat maze) > 0 then
    error "File contains invalid characters"
  else
    return maze