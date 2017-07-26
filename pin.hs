-- pin.hs
{-
    Most anything related directly to a pin lives here.
-}

module Pin
    (
       Pin (..), PinCheck(..), protoPin, makePin, showPin, openPin, printPin, savePin,
       readPins, findPin, checkPin, dropPin, scanFile, formatPinTime, check
    ) where

{-
    stack
    quickcheck
    hlint
    
    exception (IO) handling of missing .pin file
    
    V2.0
    need some kind of settings, file types, remove the pins on scan
    output colors with import System.Console.ANSI
    list formatting - make the pins display fixed width
    displaying known pins should check the hash of the file and the line
        if the pin has moved to a newline, but the hash has not changed, then
        just update the pin point
    display UTC time as local time

    commands
    "show -f " which takes a file and shows all the pins in that file
    scan file = reads a file, looks for (> and shows the user the lines
      scan file should have flags to ignore or replace known pins
      scan folder = above, but for all files in a folder
      scan folder recursive = above, but for all sub-folders
      scan here = scans the current directory use System.Directory.getCurrentDirectory
    output for the above should be a list of pins with the first 40 chars of the line, the file, the path
    list = list all pins [this should have sorting & filtering]
    list /a = list all pins with more data
    
    DONE
     new command "remove" tp delete a pin
     new command "rename" to rename a pin
     warnings of changes to file or line
     new command "check" which looks for changed files, lines, or missing files in all pins
        - use check interally before showing pin, and when listing pins
     needs hashing of file and line
     date pinned
     .pin file needs a home => will be in the same folder as the executable
     allow user to update the pinPath when a file moves? => yep

    DEFER
      read = reads a file and shows the user the contents ???? really?
      remove the protopin from the line when creating new pin
      pre-existing pin
      switch to Data.Text - is this needed? using temp files is working now.
-}

import Data.List (findIndices)
import Data.Time(getCurrentTime, UTCTime)
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.IO(openFile, IOMode(AppendMode), hPutStrLn, hClose)
import Util (hashString, hashFile, copyToTemp, pinTimeFormat, getFileContents)

data Pin = Pin {
    pinPath :: FilePath,
    pinPoint :: Int,
    pinAlias :: String,
    pinLineHash :: String,
    pinStoreTime :: UTCTime,
    pinFileHash :: String,
    pinFileTime :: UTCTime
} deriving (Show, Read)

instance Eq Pin where
    a == b = pinAlias a == pinAlias b

makePin :: FilePath -> Int -> String -> IO Pin
makePin f p s = do
                    curTime <- getCurrentTime
                    fileMod <- getModificationTime f
                    cnts <- readFile f
                    let lineHash = show $ hashString $ (lines $ cnts) !! p
                    let fileHash = show $ hashString cnts
                    return $ Pin f p s lineHash curTime fileHash fileMod

protoPin = "(>"

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

scanContents :: [String] -> [Int]
scanContents = findIndices (\x -> x == True) . map scanLine

scanFile :: FilePath -> IO [Int]
scanFile = fmap scanContents . getFileContents

savePin :: Pin -> IO ()
savePin p = do
            pins <- openFile "pin.pin" AppendMode
            hPutStrLn pins $ show p
            hClose pins
            return ()

readPins :: IO [String] -> IO [Pin]
readPins = fmap (map read)

findPin :: String -> FilePath -> IO [Pin]
findPin a f = fmap (filter (matchAlias a)) (readPins $ getFileContents f)

matchAlias :: String -> Pin -> Bool
matchAlias a p = (pinAlias p) == a

dropPin :: FilePath -> Pin -> IO ()
dropPin f p = do
                tmp <- copyToTemp f
                c <- readPins $ getFileContents tmp
                let d = filter (\x -> x /= p) c
                writeFile f $ unlines $ map show d
                removeFile tmp

checkPin :: String -> IO PinCheck
checkPin p = do
                    pins <- findPin p "pin.pin"
                    let pin = head pins
                    check pin

formatPinTime :: Pin -> String
formatPinTime = pinTimeFormat . pinStoreTime

-- File Checkss
-- 1. In same location.
-- 2. File hash mismatch
-- 3. Line hash mismatch
-- All checks will be performed and reported.
data PinCheck = PinCheckSuccess | PinCheckFileNotFound | PinCheckFileModified | PinCheckLineModified deriving (Show)

check :: Pin -> IO PinCheck
check p = do
            let f = pinPath p
            fnd <- doesFileExist f
            case fnd of
                False -> return PinCheckFileNotFound
                True  -> do
                    cnts <- readFile f
                    let lineHash = show $ hashString $ (lines $ cnts) !! (pinPoint p)
                    case lineHash == (pinLineHash p) of
                        False -> return PinCheckLineModified
                        True  -> do
                            fhash <- hashFile f
                            case fhash == (pinFileHash p) of
                                False -> return PinCheckFileModified
                                True  -> return PinCheckSuccess