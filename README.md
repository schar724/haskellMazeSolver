# haskellMazeSolver


## Haskell Modularization Tutorial:
To link files together in Haskell we want to use the import function like so:
```haskell
import Input

main :: IO ()
main = do
  maze <- readMaze "test0.txt"
  print maze
```

and then in the file thats getting imported we have to do something thats called module. I did it in the input file here:

```haskell
module Input (
              readMaze
            ) where

type Maze = [[Int]]

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
```
You can see that we create basically a list of available functions to call for other haskell files. THEN we now create an executable from them using the ghc compiler
```powershell
ghc -o myProgram Main.hs
```
After that you can run the myProgram you created and it will go through the sequencing like any imperative executable would.



## Haskell Testing Suite Tutorial
Haskell Test Suite Tutorial:
To create a test suite we need to first install hspec on your computer: 
```powershell
cabal install --lib hspec
```
Then we create a haskell file called TestMain.hs and make some tests:
```haskell
import Test.Hspec
import Input (readMaze)

main :: IO ()
main = hspec $ do
  describe "Input" $ do
    describe "readMaze" $ do
      it "reads a valid maze file correctly" $ do
        maze <- readMaze "test0.txt"
        maze `shouldBe` [[0, 0, 0, 0, 0, 0, 0, 0],
                         [0, 8, 0, 0, 0, 0, 0, 0],
                         [0, 1, 0, 0, 0, 0, 0, 0],
                         [0, 1, 1, 1, 1, 0, 0, 0],
                         [0, 0, 0, 0, 1, 0, 0, 0],
                         [0, 0, 0, 0, 1, 0, 0, 0],
                         [0, 0, 0, 0, 9, 0, 0, 0],
                         [0, 0, 0, 0, 0, 0, 0, 0]]

      it "throws an error for an empty file" $ do
        readMaze "test1.txt" `shouldThrow` anyErrorCall

      it "throws an error for a file with invalid characters" $ do
        readMaze "test2.txt" `shouldThrow` anyErrorCall
```
Then we compile it and need to also put the Input file in the compiler for some reason this time:
```powershell
ghc -o test-suite TestMain.hs Input.hs
```
Then we simply run it as an executable.
