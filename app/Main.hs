import System.IO
import Data.List

main = do
    line <- getLine
    handle <- openFile (parseCommandForFile line) ReadMode
    contents <- hGetContents handle
    putStr ((unlines . lines) contents)
    hClose handle
    main

parseCommandForFile command = if isSubsequenceOf "load" command
                              then (tail . dropWhile (/='(') . init) command
                              else takeTableFromSelect command

takeTableFromSelect content = last  (words content)