module IO (printSolutions) where
import DataTypes
import System.Console.ANSI

printSolutions :: [[Int]] -> [[Coord]] -> IO ()
printSolutions _ [] = putStr "no path found!\n"
printSolutions maze paths = do
  let maxX = length maze - 1
      maxY = length (head maze) - 1
  mapM_ (\path -> do
            let maze' = [[if (x,y) `elem` path then 'X' else if (maze !! x !! y) == 0 then '#' else if (x,y) == (0,0) then '8' else if (x,y) == (maxX,maxY) then '9' else ' ' | y <- [0..maxY]] | x <- [0..maxX]]
            mapM_ (\row -> do
                            mapM_ (\c -> do
                                        if c == 'X'
                                          then do
                                            setSGR [SetColor Foreground Vivid Green]
                                            putChar c
                                            putChar ' '
                                            setSGR [Reset]
                                          else do
                                            putChar c
                                            putChar ' '
                                  ) row
                            putStrLn ""
                   ) maze'
            putStrLn ""
        ) paths
