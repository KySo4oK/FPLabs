import System.IO
import Data.List
import Data.List.Split

main = do
    line <- getLine
    handle <- openFile (parseCommandForFile line) ReadMode
    contents <- hGetContents handle
    putStr ( if isSubsequenceOf "load" line
             then contents 
             else parseForSelectColumns (line,(words contents)))
    hClose handle
    main

parseCommandForFile command = if isSubsequenceOf "load" command
                              then (tail . dropWhile (/='(') . init) command
                              else takeTableFromSelect command

takeTableFromSelect content = last  (words content)

parseForSelectColumns (command,fileLines) = 

getListOfIndexes (command,listOfColumns) =  tail (takeWhile (/="FROM") (words command)) 

findIndexOfListElem (column,listOfColumns) =  auxFindIndexOfListElem(column,listOfColumns,0)

auxFindIndexOfListElem (column,listOfColumns,index) = if listOfColumns == []
                                             then -1 
                                             else if head listOfColumns == column
                                                  then index
                                                  else auxFindIndexOfListElem(column,(tail listOfColumns), index+1)