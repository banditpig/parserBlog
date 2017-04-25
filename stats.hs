
import Prelude hiding (sum)
import Parser
data Data = Data { name :: String, start :: Double, end :: Double, step :: Double} deriving (Show)
data OutputLine = OutputLine { functionName :: String, dataName :: String}  deriving (Show)
type Output = [OutputLine]

cmnt = many (comment "#")
comments :: Parser a -> Parser a
comments p = cmnt *> p <* cmnt

outputSectionP :: Parser [OutputLine]
outputSectionP = do
    comments (literal "output")
    many1 outputLineP

outputFunction :: String -> (Data -> Double)
outputFunction name
    | name == "mean" = mean
    | name == "sum"  = sum
    | otherwise = \_ -> 0.0
 
outputLineP :: Parser OutputLine
outputLineP = do
    funcName <- identifier
    dataName <- identifier
    return $ OutputLine  funcName dataName

dataSectionP :: Parser [Data]
dataSectionP = do
    comments (literal "data")
    many1 dataP

dataP :: Parser Data
dataP = do
    literal "name"
    nm <- identifier
    literal "start"
    st <- dbl 
    literal "end"
    nd <- dbl
    literal "step"
    stp <- dbl
    space
    return $ Data {name = nm, start = st, end = nd, step = stp}

evalOutput :: [Data] -> [OutputLine] -> IO ()
evalOutput dataList  = mapM_ (evalOutLine dataList) 

evalOutLine :: [Data] -> OutputLine -> IO ()
evalOutLine dataList ln = eval (func ln)   (dataFromName name dataList) where 
    func ln = outputFunction (functionName ln)
    name = (dataName  ln)

eval :: (Data -> Double) -> Data -> IO ()
eval f dta = print $ f dta

values :: Data -> [Double]
values dta = [f, f + dx .. l] where 
    f  = start dta
    dx = step  dta
    l  = end   dta

sum :: Data -> Double
sum dta = foldr (+) 0 (values dta)

mean :: Data -> Double
mean dta = (sum dta) / fromIntegral (length . values $ dta)

dataFromName :: String -> [Data] ->  Data
dataFromName nm dataList = dta where
    (dta:_) = filter (\(Data name _ _ _)  -> nm == name) dataList
    
main = do
   
    input <- readFile "stats.txt"
    let ((dataSet, remainder):_) = parse dataSectionP input
    print dataSet --outputSectionP
    let ((out, _):_) = parse outputSectionP remainder
    print out
    evalOutput dataSet out




