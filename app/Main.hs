import System.IO
import Data.List
import Data.List.Split

main = do
    line <- getLine
    handle <- openFile (parseCommandForFile line) ReadMode
    contents <- hGetContents handle
    putStr getContent(line,contents)
    hClose handle
    main

getContent (line, contents) = if isSubsequenceOf "load" line
                                         then contents
                                         else parseForSelectColumns (line,(words contents))

parseCommandForFile command = if isSubsequenceOf "load" command
                              then (tail . dropWhile (/='(') . init) command
                              else takeTableFromSelect command

takeTableFromSelect content = last  (words content)

parseForSelectColumns (command,fileLines) =
      let listOfIndexes = getListOfIndexes((getColumnsFromCommand command),(splitOn "," (head fileLines)))
      in map (changeLine listOfIndexes) (tail fileLines)

getColumnsFromCommand command = tail (takeWhile (/="FROM") (words command))

changeLine listOfIndexes line = let fields = splitOn "," line
                                in map (getElementByIndex fields) listOfIndexes

getElementByIndex myList index = if index == 0
                               then head myList
                               else getElementByIndex (tail myList) (index - 1)

getListOfIndexes (columns,listOfColumns) = if null columns
                                           then []
                                           else [findIndexOfListElem(head columns,listOfColumns)]:
                                           getListOfIndexes (tail columns, listOfColumns)

findIndexOfListElem (column,listOfColumns) =  auxFindIndexOfListElem(column,listOfColumns,0)

auxFindIndexOfListElem (column,listOfColumns,index) = if listOfColumns == []
                                             then -1 
                                             else if head listOfColumns == column
                                                  then index
                                                  else auxFindIndexOfListElem(column,(tail listOfColumns), index+1)