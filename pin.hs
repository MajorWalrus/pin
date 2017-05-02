-- pin.hs

{-
    needs hashing
    last access
-}

import System.IO (FilePath)

data Pin = Pin {
    pinPath :: FilePath,
    pinPoint :: Int,
    pinAlias :: String
} deriving (Show)

-- this is a function in case we want to add hashing
makePin :: FilePath -> Int -> String -> Pin
makePin f p s = Pin f p s

protoPin = "(>"

test = Pin "C://Users//sjdutch//Desktop//aa_outstanding.txt" 4 "test"

showPin :: Pin -> IO String
showPin t = printPin t $ openPin t

openPin :: Pin -> IO String
openPin t = do
                cnts <- readFile (pinPath t)
                return ((lines $ cnts) !! (pinPoint t))

printPin :: Pin -> IO String -> IO String
printPin t s = fmap (showAlias t) s

showAlias t s = (pinAlias t) ++ ": " ++ s

scanLine :: String -> Bool
scanLine = elem protoPin . words

scanFile :: [String]