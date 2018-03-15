-- pin.hs
{-
    Most anything related directly to a pin lives here.
-}

module Pin
    (
       Pin (..), PinCheck(..), protoPin, makePin, showPin, openPin, printPin, savePin,
       readPins, findPin, dropPin, scanFile, formatPinTime, check
    ) where

import Data.List (findIndices)
import Data.Time(getCurrentTime, UTCTime)
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.IO(openFile, IOMode(AppendMode), hPutStrLn, hClose)
import Util (hashString, hashFile, copyToTemp, pinTimeFormat, getFileContents, pinTimeFormat')

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

savePin :: FilePath -> Pin -> IO ()
savePin f p = do
            pins <- openFile f AppendMode
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
{-
TODO pretty sure this is unused. what was it for?
checkPin :: String -> IO PinCheck
checkPin p = do
                    pins <- findPin p "pin.pin"
                    let pin = head pins
                    check pin
-}

formatPinTime :: Pin -> IO String
formatPinTime = pinTimeFormat' . pinStoreTime

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
