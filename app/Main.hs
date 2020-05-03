import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace

main :: IO b
main = do
    line <- getLine
    if containsJoin line
    then mainWithJoin line
    else mainWithoutJoin line
    main

mainWithJoin :: String -> IO ()
mainWithJoin line = do
                  handle1 <- openFile (parseCommandForFile line) ReadMode
                  contents1 <- hGetContents handle1
                  handle2 <- openFile (parseCommandForJoinFile line) ReadMode
                  contents2 <- hGetContents handle2
                  putStr (unlines (makeJoin(line,lines contents1,lines contents2,getSeparator line)))
                  hClose handle1
                  hClose handle2

mainWithoutJoin :: String -> IO ()
mainWithoutJoin line = do
            handle <- openFile (parseCommandForFile line) ReadMode
            contents <- hGetContents handle
            putStr (unlines (checkForAggregateFunc (line,lines contents,getSeparator line)))
            hClose handle

getChangedHead :: (String, String, String) -> String
getChangedHead (file, rows, sep) = intercalate sep (map ((file ++ ".") ++) (splitOn sep rows))

makeJoin :: (String, [String], [String], String) -> [String]
makeJoin (line, fileLines1, fileLines2, sep)
             | containsRightJoin line = makeRightJoin (line, fileLines1, fileLines2, sep)
             | containsInnerJoin line = makeInnerJoin (line, fileLines1, fileLines2, sep)
             | otherwise = makeFullJoin (line, fileLines1, fileLines2, sep)

makeRightJoin :: (String, [String], [String], String) -> [String]
makeRightJoin (line, fileLines1, fileLines2, sep) = checkForAggregateFunc (
            changeCommandLikeWithoutJoin line,
            actualRightJoin (fileLines1, fileLines2, actualInnerJoin (line, fileLines1, fileLines2, sep), sep),
            sep)

actualRightJoin :: ([String], [String], [String], String) -> [String]
actualRightJoin (fileLines1, fileLines2, inner, sep) =
            inner ++
            getFullRight (fileLines2,length (splitOn sep (head fileLines1)),inner,sep)

makeFullJoin :: (String, [String], [String], String) -> [String]
makeFullJoin (line, fileLines1, fileLines2, sep) = checkForAggregateFunc (
            changeCommandLikeWithoutJoin line,
            actualFullJoin (fileLines1, fileLines2, actualInnerJoin (line, fileLines1, fileLines2, sep), sep),
            sep)

actualFullJoin :: ([String], [String], [String], String) -> [String]
actualFullJoin (fileLines1, fileLines2, inner, sep) =
            inner ++
            getFullLeft (fileLines1,length (splitOn sep (head fileLines2)),inner,sep)  ++
            getFullRight (fileLines2,length (splitOn sep (head fileLines1)),inner,sep)

getFullLeft :: ([String], Int, [String], String) -> [String]
getFullLeft (left,lr,inner,sep) = let leftPart = map (take (length (head left))) inner in
            map (++ intercalate sep (replicate lr "null")) (filter (notInPart leftPart) left)

notInPart :: [String] -> String -> Bool
notInPart [] line = True
notInPart parts line = head parts /= line && notInPart (tail parts) line

getFullRight :: ([String], Int, [String], String) -> [String]
getFullRight (right,ll,inner,sep) = let rightPart = map (drop (length  (head right))) inner in
            map (intercalate sep (replicate ll "null") ++) (filter (notInPart rightPart) right)

actualInnerJoin :: (String, [String], [String], String) -> [String]
actualInnerJoin (line, fileLines1, fileLines2, sep) =
            (getChangedHead (parseCommandForFile line,head fileLines1,sep) ++ sep ++
            getChangedHead (parseCommandForJoinFile line,head fileLines2,sep)) :
            concatMap (\l -> joinMap (fileLines2,
            findIndexOfListElem (getFirstColumnForJoinColumn (parseForJoinColumns line),
            splitOn sep (head fileLines1)),
            findIndexOfListElem (getSecondColumnForJoinColumn (parseForJoinColumns line),
            splitOn sep (head fileLines2)),
            sep,l)) fileLines1

makeInnerJoin :: (String, [String], [String], String) -> [String]
makeInnerJoin (line, fileLines1, fileLines2, sep) = checkForAggregateFunc (
            changeCommandLikeWithoutJoin line,actualInnerJoin (line, fileLines1, fileLines2, sep),sep)


changeCommandLikeWithoutJoin :: String -> String
changeCommandLikeWithoutJoin command = unwords (takeWhile isNotJoin (words command))

isNotJoin :: String -> Bool
isNotJoin line = not (isSubsequenceOf "INNER" line || isSubsequenceOf "FULL" line || isSubsequenceOf "RIGHT" line)

joinMap :: ([String], Int, Int, String, String) -> [String]
joinMap (fileLines,firstIndex,secondIndex,sep,line) =
            let rightList = findListForJoin (getElementByIndex (splitOn sep line) firstIndex,
                                             secondIndex, fileLines, sep) in
            if null rightList
            then []
            else mergeList (replicate (length rightList) line,rightList,sep)


findListForJoin :: (String, Int, [String], String) -> [String]
findListForJoin (value, index, fileLines, sep) =
            filter (\line -> getElementByIndex (splitOn sep line) index == value) fileLines

mergeList :: ([String],[String],String) -> [String]
mergeList ([],_,_) = []
mergeList (_,[],_) = []
mergeList (lines1,lines2,sep) = (head lines1 ++ sep ++ head lines2): mergeList (tail lines1,tail lines2, sep)

parseForJoinColumns :: String -> [String]
parseForJoinColumns command = tail (dropWhile (/="ON") (words command))

getFirstColumnForJoinColumn :: [String] -> String
getFirstColumnForJoinColumn columns = getColumnForJoinColumn columns head

getSecondColumnForJoinColumn :: [String] -> String
getSecondColumnForJoinColumn columns = getColumnForJoinColumn columns last

getColumnForJoinColumn :: t -> (t -> String) -> String
getColumnForJoinColumn columns func = tail (dropWhile (/='.') (func columns))

containsInnerJoin :: String -> Bool
containsInnerJoin = isSubsequenceOf "INNER"

containsRightJoin :: String -> Bool
containsRightJoin = isSubsequenceOf "RIGHT"

containsJoin :: String -> Bool
containsJoin = isSubsequenceOf "JOIN"

parseCommandForJoinFile :: String -> String
parseCommandForJoinFile command = head (tail (dropWhile (/="JOIN") (words command)))

getSeparator :: String -> String
getSeparator command = if isSubsequenceOf ".csv" command
                       then ","
                       else "\t"

getContentWithOrder :: (String, [String], String) -> [String]
getContentWithOrder (line, fileLines, sep) = if isSubsequenceOf "ORDER" line
          then map (intercalate sep) (getOrderedContent (line, fileLines, sep))
          else getContent (line, fileLines, sep)

checkForAggregateFunc :: (String, [String], String) -> [String]
checkForAggregateFunc (line, fileLines, sep)
          | containsCase line = evaluateCases (line, fileLines, sep)
          | containsCount line = replaceCountWithParam (line, fileLines, sep)
          | containsAggFunc line = replaceAggFuncWithParam (line, fileLines, sep)
          | otherwise = getContentWithOrder (line, fileLines, sep)

containsCase :: String -> Bool
containsCase = isSubsequenceOf "CASE"

evaluateCases :: (String, [String], String) -> [String]
evaluateCases (line, fileLines, sep) = let lineWithoutCase = removeCaseExp line
                                           simpleLines = checkForAggregateFunc (lineWithoutCase,fileLines,sep)
                                           caseExp = takeCaseExp line
                                       in (head simpleLines ++ sep ++ takeColumnName line) :
                                       map (\l -> mapWithCase
                                       (takeAllCasesInPairs caseExp)
                                       (takeElseCase caseExp)
                                        sep (splitOn sep (head simpleLines)) (l ++ sep)) (tail simpleLines)

mapWithCase :: [(String, String)] -> String -> Sting -> [String] -> String -> String
mapWithCase [] elseExp sep headOfFile line = line ++ elseExp
mapWithCase pairs elseExp sep headOfFile line = if evaluateSimple headOfFile fst (head pairs) (splitOn sep line)
                                                then line ++ snd (head pairs)
                                                else mapWithCase (tail pairs) elseExp sep headOfFile line

takeColumnName :: String -> String
takeColumnName line = let cond = fst (takeAllCasesInPairs (takeCaseExp line))
                      in if isSubsequenceOf "=" cond
                         then head (splitOn "=" cond)
                         else head (splitOn ">" cond)

takeColumnName :: String -> String
takeColumnName line = last (takeCaseExp line)

takeCaseExp :: String -> [String]
takeCaseExp line = takeWhile (/="FROM") (dropWhile (/="CASE") (words line))

takeAllCasesInPairs :: [String] -> [(String, String)]
takeAllCasesInPairs caseExp = let cases = takeWhile (/="ELSE") (drop 1 caseExp)
                              in auxTakeAllCasesInPairs cases

auxTakeAllCasesInPairs :: [String] -> [(String, String)]
auxTakeAllCasesInPairs [] = []
auxTakeAllCasesInPairs cases = takePair (take 4 cases) :  auxTakeAllCasesInPairs (drop 4 cases)

takePair :: [String] -> (String, String)
takePair oneCase = (getElementByIndex oneCase 1, getElementByIndex oneCase 3)

takeElseCase :: [String] -> String
takeElseCase caseExp = head (tail (dropWhile (/="ELSE") caseExp))

removeCaseExp :: String -> String
removeCaseExp line = unwords (takeWhile (/="CASE") (words line) ++ dropWhile (/="FROM") (words line))

replaceHaving :: String -> String
replaceHaving line = unwords (takeWhile (/="HAVING") (words line)
                             ++ ("WHERE" :
                             tail (dropWhile (/="HAVING") (words line))))

replaceCountWithParam :: (String, [String], String) -> [String]
replaceCountWithParam (line, fileLines, sep) = getContent (replaceHaving line,iterateGroupFunc
                                               (line,getContent(removeHavingAndCount line,fileLines,sep),sep),sep)

iterateGroupFunc :: (String, [String], String) -> [String]
iterateGroupFunc (line, fileLines, sep) = let columns = getColumnsFromCommand line
                                              gColumn = getColumnForGroupBy line
                                              gColumnIndex = findIndexOfListElem(gColumn, columns)
                                              index = findIndexOfListElem (gColumn,columns)
                                          in intercalate sep columns : getCountedGroup (gColumnIndex,
                                          length (splitOn sep (head fileLines)),
                                           map (\line -> getElementByIndex (splitOn sep line) index) (tail fileLines)
                                          , sep)

getCountedGroup :: (Int, Int, [String], String) -> [String]
getCountedGroup (index, l, [], sep) = []
getCountedGroup (index, l, mappedGB, sep) =  let gEl =  head mappedGB
                                                 count = getCount (gEl,mappedGB)
                                             in intercalate sep (replicate index (show count) ++ [gEl]
                                                ++ replicate (l - index - 1) (show count)) :
                                                getCountedGroup (index, l, drop count mappedGB,sep)

getCount :: (String, [String]) -> Int
getCount (line,lines) = if line /= head lines
                        then 1
                        else 2 + getCount(line, tail lines)

getColumnForGroupBy :: String -> String
getColumnForGroupBy line = head (tail (dropWhile (/="BY") (words line)))

removeHavingAndCount :: String -> String
removeHavingAndCount line = unwords (map (\w -> if isSubsequenceOf "COUNT" w
                                                then init (drop 6 (delete ',' w))
                                                else w ) (takeWhile (/="HAVING") (words line)))

containsCount :: String -> Bool
containsCount = isSubsequenceOf "COUNT"

replaceAggFuncWithParam :: (String, [String], String) -> [String]
replaceAggFuncWithParam (line, fileLines, sep) = iterateFunc(
                                                  getAggregateFuncFromCommand line,
                                                  tail (getContent (unwords (removeAggFunc (words line)),
                                                  fileLines, sep)), sep)

iterateFunc :: ([String], [String], String) -> [String]
iterateFunc (func, fileLines, sep) = [unwords func , intercalate sep
            (map (\fun -> show (executeFunc(fun,findIndexOfListElem(fun,func),fileLines,sep)))  func)]

executeFunc :: (String, Int, [String], String) -> Int
executeFunc (fun, index, fileLines, sep) = if isSubsequenceOf "AVG" fun
                                           then myAvg (map
                                           (\line -> read (getElementByIndex (splitOn sep line) index) :: Int)
                                           fileLines)
                                           else minimum (map
                                           (\line -> read (getElementByIndex (splitOn sep line) index) :: Int)
                                           fileLines)

myAvg :: [Int] -> Int
myAvg list = sum list `div` length list

getAggregateFuncFromCommand :: String -> [String]
getAggregateFuncFromCommand command = filter containsAggFunc (words command)

removeAggFunc :: [String] -> [String]
removeAggFunc = map (\oneCommand -> if containsAggFunc oneCommand
                                    then changeToParam oneCommand
                                    else oneCommand)

changeToParam :: String -> String
changeToParam column = if isSubsequenceOf "," column
                       then delete ')' (tail (dropWhile (/='(') column))
                       else init (tail (dropWhile (/='(') column))


containsAggFunc :: String -> Bool
containsAggFunc line = isSubsequenceOf "AVG" line || isSubsequenceOf "MIN" line

getOrderedContent :: (String, [String], String) -> [[String]]
getOrderedContent (line, fileLines, sep)
          | isSubsequenceOf "DESC" line = reverse
           (orderBy ( getListOfColumnsForOrder (init (words line)),getContent (line, fileLines, sep),sep))
          | isSubsequenceOf "ASC" line =
            orderBy (getListOfColumnsForOrder (init (words line)),getContent (line, fileLines, sep),sep)
          | otherwise =  orderBy (getListOfColumnsForOrder (words line),getContent (line, fileLines, sep),sep)

getListOfColumnsForOrder :: [String] -> [String]
getListOfColumnsForOrder command = map (delete ',') (tail (dropWhile (/="BY") command))

orderBy :: ([String], [String], String) -> [[String]]
orderBy (columns, fileLines, sep) = let indexes = getListOfIndexes (columns,splitOn sep (head fileLines))
         in sortBy (sortBy' indexes) (map (splitOn sep) fileLines)

sortBy' :: [Int] -> [String] -> [String] -> Ordering
sortBy' [] line1 line2 = LT
sortBy' indexes line1 line2 = let currentOrder = getElementByIndex line1 (head indexes) `compare`
                                                getElementByIndex line2 (head indexes)
                             in if currentOrder == EQ
                                then sortBy'  (tail indexes) line1 line2
                                else currentOrder

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
         | isSubsequenceOf "GROUP" line = if isSubsequenceOf "ORDER" line
                                          then nub (parseForSelectColumns(removeGroupBy line,fileLines,sep))
                                          else nub (parseForSelectColumns(replaceGroupBy line,fileLines,sep))
         | otherwise = parseForSelectColumns(line,fileLines,sep)

replaceGroupBy :: String -> String
replaceGroupBy line = unwords (takeWhile (/="GROUP") (words line) ++
                                           ["ORDER"] ++ init (dropWhile (/="GROUP") (words line)))

removeGroupBy :: String -> String
removeGroupBy line = unwords (takeWhile (/="GROUP") (words line) ++ dropWhile (/="ORDER") (words line))

filterFileLinesUsingWhere :: ([String], [String], String) -> [String]
filterFileLinesUsingWhere (listOfCond, fileLines, sep) =
          filter (filterLine (listOfCond, fileLines, sep))
          (tail fileLines)

filterLine :: ([String], [String], String) -> String -> Bool
filterLine (listOfCond, fileLines, sep) line = condFilter (splitOn sep (head fileLines)) listOfCond (splitOn sep line)

condFilter :: [String] -> [String] -> [String] -> Bool
condFilter headOfFile [] line = False
condFilter headOfFile listOfCond line =
          if isAndFirst listOfCond
          then evaluate headOfFile (takeFirstCond listOfCond) line &&
           condFilter headOfFile (takeRestOfCond listOfCond) line
          else evaluate headOfFile (takeFirstCond listOfCond) line ||
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
          checkMt (getElementByIndex line (findIndexOfListElem (row,headOfFile))) value

takeFirstCond :: [String] -> [String]
takeFirstCond = takeWhile isNotORorAND

takeRestOfCond :: [String] -> [String]
takeRestOfCond line = let rest = dropWhile isNotORorAND line
                 in if null rest
                    then rest
                    else tail rest

isAndFirst :: [String] -> Bool
isAndFirst listOfCond = findIndexOfListElem ("OR",listOfCond) > findIndexOfListElem ("AND",listOfCond)

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
takeAllCond wordsOfCommand =  tail (takeWhile (/="ORDER") (dropWhile (/="WHERE") wordsOfCommand))

parseCommandForFile :: String -> String
parseCommandForFile = takeTableFromSelect

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