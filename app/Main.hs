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

useWhere (line, fileLines, sep) =
         getContentWithoutWhere (line,head fileLines :
          filterFileLinesUsingWhere (takeAllCond (words line), fileLines, sep),sep)

getContentWithoutWhere (line, fileLines, sep)
         | isSubsequenceOf "load" line = fileLines
         | isSubsequenceOf "DISTINCT" line = nub (parseForSelectColumns(line,fileLines,sep))
         | otherwise = parseForSelectColumns(line,fileLines,sep)

filterFileLinesUsingWhere (listOfCond, fileLines, sep) =
          filter (\line -> condFilter (splitOn sep (head fileLines)) listOfCond (splitOn sep line))
          (tail fileLines)

condFilter headOfFile [] line = False
condFilter headOfFile listOfCond line =
          if isOrFirst listOfCond
          then (evaluate headOfFile (takeFirstCond listOfCond) line) || (condFilter headOfFile (takeRestOfCond listOfCond) line)
          else (evaluate headOfFile (takeFirstCond listOfCond) line) && (condFilter headOfFile (takeRestOfCond listOfCond) line)

evaluate headOfFile condition line = if isSubsequenceOf ["NOT"] condition
                                     then not (evaluateSimple headOfFile (last condition) line)
                                     else evaluateSimple headOfFile (head condition) line

evaluateSimple headOfFile condition line =
          if isSubsequenceOf "=" condition
          then let lc = splitOn "=" condition in checkEqCond headOfFile (head lc) (last lc) line
          else let lc = splitOn ">" condition in checkMtCond headOfFile (head lc) (last lc) line

checkEqCond headOfFile row value line =
          checkEq value (getElementByIndex line (findIndexOfListElem (row,headOfFile)))

checkMtCond headOfFile row value line =
          checkMt value (getElementByIndex line (findIndexOfListElem (row,headOfFile)))

takeFirstCond = takeWhile isNotORorAND

takeRestOfCond line = let rest = dropWhile isNotORorAND line
                 in if null rest
                    then rest
                    else tail rest

isOrFirst listOfCond = findIndexOfListElem ("AND",listOfCond) > findIndexOfListElem ("OR",listOfCond)

isNotORorAND cond = not (cond=="AND" || cond=="OR")

checkEq a b = if isNumber' a
              then (read a + 0.0) == (read b + 0.0)
              else a==b

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

takeAllCond wordsOfCommand =  tail (dropWhile (/="WHERE") wordsOfCommand)

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

getElementByIndex [] index = error "Empty list exc"
getElementByIndex myList index = if index == 0
                               then head myList
                               else getElementByIndex (tail myList) (index - 1)

getListOfIndexes (columns,listOfColumns) = if null columns
                                           then []
                                           else findIndexOfListElem (head columns,listOfColumns):
                                           getListOfIndexes (tail columns, listOfColumns)

findIndexOfListElem (column,listOfColumns) =  auxFindIndexOfListElem(column,listOfColumns,0)

auxFindIndexOfListElem (column,listOfColumns,index)
              | null listOfColumns = maxBound :: Int
              | head listOfColumns == column = index
              | otherwise = auxFindIndexOfListElem (column,tail listOfColumns, index+1)