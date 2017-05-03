-- pin.hs

{-
    needs hashing
    last access
    cmnd line interface
    .pin file needs a home
-}

import System.IO (FilePath)
import Data.List (findIndices)
import System.IO

data Pin = Pin {
    pinPath :: FilePath,
    pinPoint :: Int,
    pinAlias :: String
} deriving (Show, Read)

-- this is a function in case we want to add hashing
makePin :: FilePath -> Int -> String -> Pin
makePin f p s = Pin f p s

protoPin = "(>"

test = Pin "C://Users//sjdutch//Desktop//aa_outstanding.txt" 4 "test"
test2 = Pin "C://Users//sjdutch//Desktop//aa_outstanding.txt" 31 "burden"
test3 = Pin "C://Users//sjdutch//Desktop//aa_outstanding.txt" 11 "mostly"

showPin :: Pin -> IO String
showPin t = printPin t $ openPin t

openPin :: Pin -> IO String
openPin t = do
                cnts <- readFile (pinPath t)
                return ((lines $ cnts) !! (pinPoint t))

printPin :: Pin -> IO String -> IO String
printPin t s = fmap (showAlias t) s

showAlias t s = (pinAlias t) ++ ": " ++ s

-- need something to remove the protoPin from the string before displaying
--cleanAlias :: String -> String
--cleanAlias = dropWhile (\x -> x == protoPin) . words

scanLine :: String -> Bool
scanLine = elem protoPin . words

scanContents :: [String] -> [Int]
scanContents = findIndices (\x -> x == True) . map scanLine

scanFile :: FilePath -> IO [Int]
scanFile = fmap scanContents . getFileContents

getFileContents :: FilePath -> IO [String]
getFileContents = fmap lines . readFile

savePin :: Pin -> IO()
savePin p = do
            pins <- openFile "pin.pin" AppendMode
            hPutStrLn pins $ show p
            hClose pins
            return ()

readPins :: IO [String] -> IO [Pin]
readPins = fmap (map read)