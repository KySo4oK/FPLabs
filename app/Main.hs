import System.IO
import Data.List

main = do
    line <- getLine
    handle <- openFile (parseCommand line) ReadMode
    contents <- hGetContents handle
    putStr ((unlines . lines) contents)
    hClose handle
    main

parseCommand = tail . dropWhile (/='(') . init