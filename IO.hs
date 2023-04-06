module IO (printSolutions) where
import DataTypes



-- readMaze :: String -> Maze
-- readMaze contents = do
--   let maze = map (map read . words) $ lines contents
--   if (length maze == 0) || (length (head maze) == 0) then
--     error "File is empty"
--   else if (length $ filter (\x -> x `notElem` [0,1,8,9]) $ concat maze) > 0 then
--     error "File contains invalid characters"
--   else
--     return maze

-- printSolution :: [[Coord]] -> IO ()
-- printSolution solutions =
--   mapM_ (\solution -> mapM_ putStrLn (createMaze solution ++ [""])) solutions

-- -- | Takes a list of coordinates representing a solution path through a maze, and returns a list of strings
-- --   representing the maze with walls represented by '#' and the path represented by 'X'. The start and exit
-- --   points are also represented by 'X'.
-- createMaze :: [Coord] -> [String]
-- createMaze solution =
--   let (maxX, maxY) = foldl (\(mx, my) (x, y) -> (max mx x, max my y)) (0, 0) solution
--       isPath x y = (x, y) `elem` solution
--       isStartOrExit x y = (x, y) == head solution || (x, y) == last solution
--       isWall x y = not (isPath x y || isStartOrExit x y)
--       charFor x y
--         | isWall x y = '#'
--         | isStartOrExit x y = 'X'
--         | isPath x y = 'X'
--         | otherwise = ' '
--   in [ [ charFor x y | x <- [0..maxX] ] | y <- [0..maxY] ]

printSolutions :: [[Int]] -> [[Coord]] -> IO ()
printSolutions _ [] = putStr "no path found!\n"
printSolutions maze paths = do
  let maxX = length maze - 1
      maxY = length (head maze) - 1
  mapM_ (\path -> do
            let maze' = [[if (x,y) `elem` path then 'X' else if (maze !! x !! y) == 0 then '#' else if (x,y) == (0,0) then '8' else if (x,y) == (maxX,maxY) then '9' else ' ' | y <- [0..maxY]] | x <- [0..maxX]]
            mapM_ (\row -> putStrLn (concatMap (\c -> [c, ' ']) row)) maze'
            putStrLn ""
        ) paths