
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
import System.IO 
import Parser
import Control.Applicative hiding (many)

data PackageDec = Pkg [String] deriving (Show)
data ClassDec   = Clz String deriving (Show)
data VarDec     = Var VarName VarVal VarType deriving (Show)
data VarVal     = Simple String | Bol String | Arry  [String]  deriving (Show)
data VarType    = Strng  | Intgr | Booln deriving (Show, Eq)
type VarName    = String 
data Lines      = Lines { packageDec :: PackageDec, classDec :: ClassDec, varDecs :: [VarDec] } deriving (Show)

cmnt = many (comment "#")
cmnts :: Parser a -> Parser a
cmnts p = cmnt *> p <* cmnt

linesP :: Parser Lines
linesP = do
    packageDec <- cmnts packageP
    classDec   <- cmnts  classP
    varDecs    <- varDecsP
    
    return $ Lines packageDec classDec varDecs

linesP' :: Parser Lines
linesP' = Lines <$> packageP <*>  classP <*>  varDecsP


packageP :: Parser PackageDec
packageP = do 
    literal "package"
    x  <- identifier
    xs <- many ((:) <$> char '.' *> identifier)
    return $ Pkg $ (x:xs) 

packageP' :: Parser PackageDec
packageP' = literal "package" >> Pkg <$> ((:) <$> identifier <*> identifiers)
    where 
        identifiers = many ((:) <$> char '.' *> identifier)
 
classP :: Parser ClassDec
classP = do
    literal "class"
    x  <- upperCase
    xs <- many alpha
    return $ Clz (x:xs)

classP' :: Parser ClassDec
classP' = Clz <$> ( (:) <$> upperCase <*> many alpha)

varDecsP :: Parser [VarDec]
varDecsP = many (cmnts  varDecP)


varDecP :: Parser VarDec
varDecP = do
    vName <-  identifier
    literal "="
    vVal  <- varValP
    literal "::"
    vType <- varTypeP
    return $ Var vName vVal vType

varValP :: Parser VarVal 
varValP = go where
    go = do 
             list <- literal "[" >> (:) <$> anyString <*> many (literal "," >> anyString) <* literal "]"
             return $ Arry list  
         <|> 
            do
             vVal <- anyString
             return $ Simple vVal      
         <|>
            do 
             bool <- (literal "True" <|> literal "False")
             return $ Bol bool

varTypeP :: Parser VarType
varTypeP = do
    vt <- anyString
    case vt of
        "String"  -> return Strng
        "Int"     -> return Intgr
        "Boolean" -> return Booln
        _         -> return Booln

strType :: VarType -> String
strType Strng  = "String"
strType Booln  = "Boolean"
strType Intgr  = "Integer"

evalVarDec :: VarDec -> String
evalVarDec (Var name val tp) = eval val where
     eval :: VarVal -> String
     eval (Simple str) = "\tfinal " ++ strType tp ++ " "   ++ name ++ " = " ++ evalVal str tp ++ ";"
     eval (Bol    str) = "\tfinal " ++ strType tp ++ " "   ++ name ++ " = " ++ evalVal str tp ++ ";"
     eval (Arry   arr) = "\tfinal " ++ strType tp ++ "[] " ++ name ++ " = " ++ evalArr arr tp ++ "};"
     evalVal str' tp'
       | tp' == Intgr = str'
       | tp' == Booln = str'
       | tp' == Strng = "\"" ++ str' ++ "\""
     evalArr arr' tp' = "new " ++ strType tp ++ "[]{" ++ toString arr' tp' where
        toString :: [String] -> VarType -> String
        -- Integer or Boolean array get rid of "
        toString arr' tp' = foldr repl [] (show arr') where
          repl x ac 
            | x == ']' || x == '[' = ac
            | x == '"' && tp' == Intgr = ac
            | x == '"' && tp' == Booln = ac
            | x == ','                 = ',':' ':ac
            | otherwise                = x:ac
            

writeOutput :: Lines -> IO ()
writeOutput lns = do
    
    let (Clz fName) = classDec lns
    h <- openFile (fName ++ ".java") WriteMode

    let (Pkg (x:xs)) = packageDec lns
    hPutStr h "package " >> hPutStr h x >> mapM_ ( hPutStr h . ("." ++)) xs >> hPutStrLn h ";"

    let (Clz clName) = classDec lns
    hPutStr h "public class " >> hPutStr h clName >> hPutStrLn h "{"

    hPutStr h  $ unlines . map evalVarDec . varDecs $ lns

    hPutStr h "}"
    hClose h

main :: IO ()
main = do
    input <- readFile "test.txt"
    let ((res, _):_) = parse linesP input 
    writeOutput res
