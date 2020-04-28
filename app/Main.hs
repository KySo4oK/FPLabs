import System.IO
import Data.List
import Data.List.Split
import Data.Char

main = do
    line <- getLine
    handle <- openFile (parseCommandForFile line) ReadMode
    contents <- hGetContents handle
    putStr (unlines (getContent(line,lines contents,getSeparator line)))
    hClose handle
    main

getSeparator command = if isSubsequenceOf ".csv" command
                       then ","
                       else "\t"

getContent (line, fileLines, sep)
         | isSubsequenceOf "WHERE" line = useWhere (line, fileLines, sep)
         | otherwise = getContentWithoutWhere (line, fileLines, sep)

getContentWithoutWhere (line, fileLines, sep)
         | isSubsequenceOf "load" line = fileLines
         | isSubsequenceOf "DISTINCT" line = nub (parseForSelectColumns(line,fileLines,sep))
         | otherwise = parseForSelectColumns(line,fileLines,sep)

filterFileLinesUsingWhere (condition, fileLines, sep) =
          if isSubsequenceOf "=" condition
          then let lc = splitOn "=" condition in eqFilter (head lc,last lc,fileLines,sep)
          else let lc = splitOn ">" condition in mtFilter (head lc,last lc,fileLines,sep)

eqFilter (column,value,fileLines,sep) =
          let indexOfCond = findIndexOfListElem (column,splitOn sep (head fileLines))
          in filter (\line ->  checkEq (getElementByIndex (splitOn sep line) indexOfCond) value ) fileLines

checkEq a b = if isNumber' a
              then (read a + 0.0) == (read b + 0.0)
              else a==b

mtFilter (column,value,fileLines,sep) =
          let indexOfCond = findIndexOfListElem (column,splitOn sep (head fileLines))
          in filter (\line -> checkMt (getElementByIndex (splitOn sep line) indexOfCond) value ) fileLines

checkMt a b = if isNumber' a
              then (read a + 0.0) > (read b + 0.0)
              else a>b

isNumber' :: String -> Bool
isNumber' ""  = False
isNumber' "." = False
isNumber' xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

useWhere (line, fileLines, sep) =
         getContentWithoutWhere (line,filterFileLinesUsingWhere (last (words line), fileLines, sep),sep)

parseCommandForFile command = if isSubsequenceOf "load" command
                              then (tail . dropWhile (/='(') . init) command
                              else takeTableFromSelect command

takeTableFromSelect content = head  (tail (dropWhile (/="FROM") (words content)))

parseForSelectColumns (command,fileLines,sep) =
      let listOfIndexes = getListOfIndexes (getColumnsFromCommand command,splitOn sep (head fileLines))
      in map (\line -> changeLine (listOfIndexes,line,sep)) fileLines

getColumnsFromCommand command = filter (/="DISTINCT")(map (delete ',') (tail (takeWhile (/="FROM") (words command))))


changeLine (listOfIndexes,line,sep) = let fields = splitOn sep line
                                in intercalate sep (map (getElementByIndex fields) listOfIndexes)

getElementByIndex myList index = if index == 0
                               then head myList
                               else getElementByIndex (tail myList) (index - 1)

getListOfIndexes (columns,listOfColumns) = if null columns
                                           then []
                                           else findIndexOfListElem (head columns,listOfColumns):
                                           getListOfIndexes (tail columns, listOfColumns)

findIndexOfListElem (column,listOfColumns) =  auxFindIndexOfListElem(column,listOfColumns,0)

auxFindIndexOfListElem (column,listOfColumns,index)
              | null listOfColumns = error "Element not found exc"
              | head listOfColumns == column = index
              | otherwise = auxFindIndexOfListElem (column,tail listOfColumns, index+1)