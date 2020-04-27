import System.IO

main = do
    handle <- openFile "f1.csv" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle