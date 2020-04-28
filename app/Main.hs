import System.IO
import Data.List
import Data.List.Split

main = do
    line <- getLine
    handle <- openFile (parseCommandForFile line) ReadMode
    contents <- hGetContents handle
    putStr ((unlines (getContent(line,(lines contents)))))
    hClose handle
    main

getContent (line, fileLines) = if isSubsequenceOf "load" line
                                         then fileLines
                                         else parseForSelectColumns(line,fileLines)

parseCommandForFile command = if isSubsequenceOf "load" command
                              then (tail . dropWhile (/='(') . init) command
                              else takeTableFromSelect command

takeTableFromSelect content = last  (words content)

parseForSelectColumns (command,fileLines) =
      let listOfIndexes = getListOfIndexes ((getColumnsFromCommand command),(splitOn "," (head fileLines)))
      in map (\line -> changeLine (listOfIndexes,line)) fileLines

getColumnsFromCommand command = map (delete ',') (tail (takeWhile (/="FROM") (words command)))


changeLine (listOfIndexes,line) = let fields = splitOn "," line
                                in intercalate ","(map (getElementByIndex fields) listOfIndexes)

getElementByIndex myList index = if index == 0
                               then head myList
                               else getElementByIndex (tail myList) (index - 1)

getListOfIndexes (columns,listOfColumns) = if null columns
                                           then []
                                           else (findIndexOfListElem(head columns,listOfColumns)):
                                           getListOfIndexes (tail columns, listOfColumns)

findIndexOfListElem (column,listOfColumns) =  auxFindIndexOfListElem(column,listOfColumns,0)

auxFindIndexOfListElem (column,listOfColumns,index)
              | null listOfColumns = error "Element not found"
              | head listOfColumns == column = index
              | otherwise = auxFindIndexOfListElem(column,(tail listOfColumns), index+1)