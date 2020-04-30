import System.IO
import Data.List
import Data.List.Split
import Data.Char

main :: IO b
main = do
    line <- getLine
    handle <- openFile (parseCommandForFile line) ReadMode
    contents <- hGetContents handle
    putStr (unlines (getContent(line,lines contents,getSeparator line)))
    hClose handle
    main

getSeparator :: String -> String
getSeparator command = if isSubsequenceOf ".csv" command
                       then ","
                       else "\t"

getContent :: (String, [String], String) -> [String]
getContent (line, fileLines, sep)
         | isSubsequenceOf "WHERE" line = useWhere (line, fileLines, sep)
         | otherwise = getContentWithoutWhere (line, fileLines, sep)

useWhere :: (String, [String], String) -> [String]
useWhere (line, fileLines, sep) =
         getContentWithoutWhere (line,head fileLines :
          filterFileLinesUsingWhere (takeAllCond (words line), fileLines, sep),sep)

getContentWithoutWhere :: (String, [String], String) -> [String]
getContentWithoutWhere (line, fileLines, sep)
         | isSubsequenceOf "load" line = fileLines
         | isSubsequenceOf "DISTINCT" line = nub (parseForSelectColumns(line,fileLines,sep))
         | otherwise = parseForSelectColumns(line,fileLines,sep)

filterFileLinesUsingWhere :: ([String], [String], String) -> [String]
filterFileLinesUsingWhere (listOfCond, fileLines, sep) =
          filter (filterLine (listOfCond, fileLines, sep))
          (tail fileLines)

filterLine :: ([String], [String], String) -> String -> Bool
filterLine (listOfCond, fileLines, sep) line = condFilter (splitOn sep (head fileLines)) listOfCond (splitOn sep line)

condFilter :: [String] -> [String] -> [String] -> Bool
condFilter headOfFile [] line = False
condFilter headOfFile listOfCond line =
          if isOrFirst listOfCond
          then evaluate headOfFile (takeFirstCond listOfCond) line ||
           condFilter headOfFile (takeRestOfCond listOfCond) line
          else evaluate headOfFile (takeFirstCond listOfCond) line &&
           condFilter headOfFile (takeRestOfCond listOfCond) line

evaluate :: [String] -> [String] -> [String] -> Bool
evaluate headOfFile condition line = if isSubsequenceOf ["NOT"] condition
                                     then not (evaluateSimple headOfFile (last condition) line)
                                     else evaluateSimple headOfFile (head condition) line

evaluateSimple :: [String] -> String -> [String] -> Bool
evaluateSimple headOfFile condition line =
          if isSubsequenceOf "=" condition
          then let lc = splitOn "=" condition in checkEqCond headOfFile (head lc) (last lc) line
          else let lc = splitOn ">" condition in checkMtCond headOfFile (head lc) (last lc) line

checkEqCond :: Eq a => [a] -> a -> String -> [String] -> Bool
checkEqCond headOfFile row value line =
          checkEq value (getElementByIndex line (findIndexOfListElem (row,headOfFile)))

checkMtCond :: Eq a => [a] -> a -> String -> [String] -> Bool
checkMtCond headOfFile row value line =
          checkMt value (getElementByIndex line (findIndexOfListElem (row,headOfFile)))

takeFirstCond :: [String] -> [String]
takeFirstCond = takeWhile isNotORorAND

takeRestOfCond :: [String] -> [String]
takeRestOfCond line = let rest = dropWhile isNotORorAND line
                 in if null rest
                    then rest
                    else tail rest

isOrFirst :: [String] -> Bool
isOrFirst listOfCond = findIndexOfListElem ("AND",listOfCond) > findIndexOfListElem ("OR",listOfCond)

isNotORorAND :: String -> Bool
isNotORorAND cond = not (cond=="AND" || cond=="OR")

checkEq :: String -> String -> Bool
checkEq a b = if isNumber' a
              then (read a + 0.0) == (read b + 0.0)
              else a==b

checkMt :: String -> String -> Bool
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

takeAllCond :: [String] -> [String]
takeAllCond wordsOfCommand =  tail (dropWhile (/="WHERE") wordsOfCommand)

parseCommandForFile :: String -> String
parseCommandForFile command = if isSubsequenceOf "load" command
                              then (tail . dropWhile (/='(') . init) command
                              else takeTableFromSelect command

takeTableFromSelect :: String -> String
takeTableFromSelect content = head  (tail (dropWhile (/="FROM") (words content)))

parseForSelectColumns :: (String, [String], String) -> [String]
parseForSelectColumns (command,fileLines,sep) =
      let listOfIndexes = getListOfIndexes (getColumnsFromCommand command,splitOn sep (head fileLines))
      in map (\line -> changeLine (listOfIndexes,line,sep)) fileLines

getColumnsFromCommand :: String -> [String]
getColumnsFromCommand command = filter (/="DISTINCT")(map (delete ',') (tail (takeWhile (/="FROM") (words command))))


changeLine :: (Num t, Eq t, Eq a) => ([t], [a], [a]) -> [a]
changeLine (listOfIndexes,line,sep) = let fields = splitOn sep line
                                in intercalate sep (map (getElementByIndex fields) listOfIndexes)

getElementByIndex :: (Eq t, Num t) => [a] -> t -> a
getElementByIndex [] index = error "Empty list exc"
getElementByIndex myList index = if index == 0
                               then head myList
                               else getElementByIndex (tail myList) (index - 1)

getListOfIndexes :: Eq a => ([a], [a]) -> [Int]
getListOfIndexes (columns,listOfColumns) = if null columns
                                           then []
                                           else findIndexOfListElem (head columns,listOfColumns):
                                           getListOfIndexes (tail columns, listOfColumns)

findIndexOfListElem :: Eq a => (a, [a]) -> Int
findIndexOfListElem (column,listOfColumns) =  auxFindIndexOfListElem(column,listOfColumns,0)

auxFindIndexOfListElem :: Eq a => (a, [a], Int) -> Int
auxFindIndexOfListElem (column,listOfColumns,index)
              | null listOfColumns = maxBound :: Int
              | head listOfColumns == column = index
              | otherwise = auxFindIndexOfListElem (column,tail listOfColumns, index+1)