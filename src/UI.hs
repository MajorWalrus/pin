-- ui.hs
{-
    UI is where all the user actions and reactions live.
-}

module UI
    (
       cmdList, cmdShow, cmdScan, cmdDel, cmdRename, cmdUpdateHashes, cmdHelp, cmdQuit, cmdDetail, cmdPath, cleanPath, cmdOpen
    ) where

import Data.List (intercalate)
import System.Directory (getModificationTime, doesFileExist)
import Pin (Pin(..), PinCheck(..), protoPin, readPins, findPin, showPin, dropPin, scanFile, savePin, makePin, formatPinTime, check)
import Util (copyToTemp, replace, hashString, pinTimeFormat, pinTimeFormat', getFileContents)
import Confirm
import System.Process (spawnCommand, ProcessHandle)
import Control.Exception

-- list

cmdList :: FilePath -> IO ()
cmdList f = (readPins $ getFileContents f) >>= mapM_ (putStrLn . displayPin)

displayPin :: Pin -> String
displayPin p = (pinAlias p) ++ (pad protoPin) ++ (pinPath p)

pad :: String -> String
pad "" = ""
pad s = " " ++ s ++ " "

-- show

cmdShow :: [String] -> FilePath -> IO ()
cmdShow ps f = mapM_ (cmdShow' f) ps

cmdShow' :: FilePath -> String -> IO ()
cmdShow' f a = do
            pins <- findPin a f
            let okpins = map pinOk pins
            mapM_ (>>= putStrLn) (map (>>= buildShowPin) okpins)
{-
    The above is the shortest I can make this function. The types don't line up, so I can't figure out how 
    to do it in one line.
        findPin :: String -> FilePath -> IO [Pin]
        map showPin :: [Pin] -> [IO String]

    The second line means, "Call buildShowPin on every pin that was found. Then bind the result to putstring 
    by calling that function for every string and throwing away the resulting list."
-}

buildShowPin :: (Maybe Pin, IO String) -> IO String
buildShowPin (Just p, s) = join (cleanPinInLine <$> showPin p) s
buildShowPin (Nothing, s) = s

pinOk :: Pin -> IO (Maybe Pin, IO String)
pinOk p = do
            status <- check p
            case status of
                PinCheckSuccess      -> return ((Just p), return "") 
                PinCheckFileModified -> return ((Just p), fmap ("\n    The file this pin points to has been modified." ++) $ join (pinPinnedTime p) (pinFileModTime p))
                PinCheckLineModified -> return ((Just p), fmap ("\n    The line this pin points to has been modified." ++) $ join (pinPinnedTime p) (pinFileModTime p))
                PinCheckFileNotFound -> return (Nothing, return "The file was moved or deleted.")

pinPinnedTime :: Pin -> IO String
pinPinnedTime p = fmap ("\n      Pinned:        " ++) $ formatPinTime p
-- sting ++ io string -> IO string

pinFileModTime :: Pin -> IO String
pinFileModTime p = fmap ("\n      File Changed:  " ++) $ (getModificationTime $ pinPath p) >>= pinTimeFormat'


join :: IO String -> IO String -> IO String
join s1 s2 = (++) <$> s1 <*> s2

-- need something to remove the protoPin from the string before displaying
cleanPinInLine :: String -> String
cleanPinInLine = intercalate " " . filter (/= protoPin) . words

-- scan

-- future data ScanOptions = RemovePin | LeavePin

{-
    scanning one file
    1. find pins in file
    2. if no pins found -> print message and end
    3. otherwise for each in, display the line, prompt user for alias, and save
-}

-- TODO need to handle pins that already exist
cmdScan :: FilePath -> [String] -> FilePath -> IO ()
cmdScan f [] ps = pinFromFile (cleanPath f) ps
cmdScan f (x:xs) ps = putStrLn "Scanning args not implemented."

pinFromFile :: FilePath -> FilePath -> IO ()
pinFromFile f ps = do 
                p <- scanFile f -- TODO this should return a Maybe because if the file is not found, we get an exception
                -- what if we match the list of found pins against known pins
                case length p of
                    0 -> putStrLn "No pins found."
                    otherwise -> do
                                    putStrLn $ show (length p) ++ " pins found. Enter an alias."
                                    -- here's the current place to short-circut the creation of a pin if it already exists.
                                    -- but, should the user be notified that pins aready exist in the scan file?
                                    --newPin <- promptForAlias f p 
                                    mapM_ (>>= savePin ps) $ map (promptForAlias f) p -- TODO promptForAliaas should just return IO string
                                    

promptForAlias :: FilePath -> Int -> IO Pin
promptForAlias f l = 
    getLineFromFile f l >>=
    (\line -> putStrLn $ "   File: " ++ f ++ "\n   Line: " ++ (stripNewLine line)) >>
    getLine >>= 
    (\alias -> makePin f l alias)

-- TODO this needs to be used on the other read functions
getLineFromFile :: FilePath -> Int -> IO String
getLineFromFile f l = do
                cnts <- readFile f
                return ((lines $ cnts) !! l)


stripNewLine :: String -> String
stripNewLine = filter (/= '\n')

--- Future

removePinFromLine :: FilePath -> Int -> IO ()
removePinFromLine f l = do
                oldLine <- getLineFromFile f l
                let newLine = cleanPinInLine oldLine
                updatePinFile f oldLine newLine

-- delete

cmdDel :: String -> FilePath -> IO ()
cmdDel "" _ = putStrLn "Please name the pin to delete."
cmdDel p f = do
                tmp <- copyToTemp f
                ps <- findPin p tmp
                delete f ps 

delete :: FilePath -> [Pin] -> IO ()
delete f (x:[]) = dropPin f x
delete f (x:xs) = delConfrim where
                        delConfrim = confirm yesDeleteAll noDeleteAll $ "Multiple pins exist with the alias '" ++ (pinAlias x) ++ "'. Delete all?"
                        onDelYes = dropPin f x
                        onDelNo  = putStrLn "Deletion canceled."
                        yesDeleteAll = Confirm onDelYes "Y"
                        noDeleteAll =  Confirm onDelNo  "N"

-- rename

-- TODO watch out! this has been hard-coded to work with the file structure, pinAlias = "lastly"}
cmdRename :: String -> String -> FilePath -> IO ()
cmdRename old new f = updatePinFile f ("\"" ++ old ++ "\",") ("\"" ++ new ++ "\",")

updatePinFile :: FilePath -> String -> String -> IO ()
updatePinFile f old new = do
                        tmp <- copyToTemp f
                        cnt <- getFileContents tmp
                        writeFile f $ unlines $ map (update . words) cnt where
                            update line = intercalate " " $ replace old new line

-- update

-- Updating hashes is weird for files that are only one line. The file hash and the line hash are the same.
cmdUpdateHashes :: String -> FilePath -> IO ()
cmdUpdateHashes p f= do 
                        tmp <- copyToTemp f
                        pins <- findPin p tmp
                        mapM_ (updateHashes f) pins

updateHashes :: FilePath -> Pin -> IO ()
updateHashes f p = do 
                let old_lh = pinLineHash p
                let old_fh = pinFileHash p
                cnts <- readFile $ pinPath p
                let new_lh = show $ hashString $ (lines $ cnts) !! (pinPoint p)
                let new_fh = (show . hashString) cnts
                updatePinFile f ("\"" ++ old_lh ++ "\",") ("\"" ++ new_lh ++ "\",")
                updatePinFile f ("\"" ++ old_fh ++ "\",") ("\"" ++ new_fh ++ "\",")

-- path

cmdPath :: String -> FilePath -> FilePath -> IO ()
cmdPath p n f = do
                tmp <- copyToTemp f
                pins <- findPin p tmp
                case pins of 
                    (p:[]) -> mapM_ (updatePath f n) pins
                    (p:ps) -> pathConfrim where
                        pathConfrim = confirm yesPathAll noPathAll $ "Multiple pins exist with the alias '" ++ (pinAlias p) ++ "'. Repath all to " ++ n ++ "?"
                        onPathYes = mapM_ (updatePath f n) pins
                        onPathNo  = putStrLn "Repathing canceled."
                        yesPathAll = Confirm onPathYes "Y"
                        noPathAll =  Confirm onPathNo  "N"

updatePath :: FilePath -> FilePath -> Pin -> IO ()
updatePath f n p = do
                    let old_path = pinPath p
                    let cp = cleanPath n
                    fnd <- doesFileExist cp
                    case fnd of
                        True -> updatePinFile f ("\"" ++ old_path ++ "\",") ("\"" ++ n ++ "\",")
                        False -> putStrLn $ "The file " ++ n ++ " could not be found."

cleanPath :: String -> String
cleanPath =  map (\c -> if c == '\\' then '/'; else c)

-- detail

cmdDetail :: String -> FilePath -> IO ()
cmdDetail p f = do
                pins <- findPin p f
                let detes = map pinDetail pins
                mapM_ (>>= putStrLn) detes 

pinDetail :: Pin -> IO String
pinDetail p = fmap (("Alias: " ++ (pinAlias p) ++ "\n    File: " ++ (pinPath p) ++ "\n    Line: " ++ (show $ pinPoint p) ++ "\n    Pinned: ") ++) $ formatPinTime p

-- open

cmdOpen :: String -> FilePath -> IO ()
cmdOpen p d = do
                    tmp <- copyToTemp d
                    ps <- findPin p tmp
                    mapM_ (openPinFile) ps

openPinFile :: Pin -> IO ()
openPinFile p = do
                let f = pinPath p
                fnd <- doesFileExist f
                case fnd of
                    False -> putStrLn $ "The file " ++ f ++ " could not be found."
                    True -> do
                                result <- try (spawnCommand f) :: IO (Either SomeException ProcessHandle)
                                case result of
                                    Left _ -> putStrLn $ "An error has occurred opening the file."
                                    Right val -> putStrLn $ "Opening " ++ f ++ " from " ++ pinAlias p ++ "."
-- help
cmdHelp :: IO ()
cmdHelp = putStrLn "pin is a utility which allows you to put structured metadata (pins) \nin text files. \n\n   Version 0.3"

-- quit
cmdQuit :: IO ()
cmdQuit = return () --TODO exitSuccess?