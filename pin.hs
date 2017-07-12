-- pin.hs

{-
    stack
    quickcheck
    switch to Data.Text - is this needed? using temp files is working now.
    hlint
    seperate code into modules
    
    exception (IO) handling
    pre-existing pins
    remove the protopin from the line when creating new pin
    allow user to update the pinPath when a file moves?
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

    DEFER
      read = reads a file and shows the user the contents ???? really?
-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (findIndices, intercalate)
import System.IO
import System.Environment
import System.Exit
import System.Directory (doesDirectoryExist, doesFileExist, removeFile, getModificationTime, getDirectoryContents)
import Data.Time(getCurrentTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

--import Options.Applicative
--import System.Console.ANSI as ANSI

import "cryptonite" Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256(..))
import Data.ByteString.Char8 as C8 (pack)

hashString :: String -> Digest SHA256
hashString = hash . C8.pack

hashFile :: FilePath -> IO String
hashFile f = fmap (show . hashString) $ readFile f

compareUTC :: IO UTCTime -> IO UTCTime -> IO Bool
compareUTC a b = do 
                    a' <- a
                    b' <- b
                    return $ a' == b'

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
tempPattern = "~_"

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

copyToTemp :: FilePath -> IO FilePath
copyToTemp f = do
                (tmpFile, tmpHand) <- openTempFile "." (tempPattern ++ f)
                hClose tmpHand
                cons <- readFile f
                writeFile tmpFile cons
                return tmpFile


main = do
    --allpins
    cleanUpTemps "."
    (fmap parseArgs $ getArgs) >>= run
    
    --cmd <- fmap parseArgs $ getArgs
    --run cmd
    --getProgName >>= print
    --getEnvironment >>= print

---fmap (map putStrLn) $ getArgs

data PinCommand = CommandList | CommandShow {cmdPinAlias :: [String] } | 
    CommandScan { scanPath :: FilePath, scanFlags :: [String] } | CommandHelp | 
    CommandQuit | CommandDelete { delPin :: String} | CommandRename {oldPin :: String, newPin :: String} |
    CommandUpdate { updatePin :: String }
    deriving (Show)

type ScanArgs = String

pad :: String -> String
pad "" = ""
pad s = " " ++ s ++ " "

displayPin :: Pin -> String
displayPin p = (pinAlias p) ++ (pad protoPin) ++ (pinPath p)

cmdList :: FilePath -> IO ()
cmdList f = (readPins $ getFileContents f) >>= mapM_ (putStrLn . displayPin)

cmdShow :: [String] -> IO ()
cmdShow = mapM_ (cmdShow' "pin.pin")

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
buildShowPin (Just p, s) = join (showPin p) s
bulidShowPin (Nothing, s) = return s

pinOk :: Pin -> IO (Maybe Pin, IO String)
pinOk p = do
            status <- check p
            case status of
                PinCheckSuccess      -> return ((Just p), return "") 
                PinCheckFileModified -> return ((Just p), fmap ("\n    The file this pin points to has been modified." ++) $ join (pinPinnedTime p) (pinFileModTime p))
                PinCheckLineModified -> return ((Just p), fmap ("\n    The line this pin points to has been modified." ++) $ join (pinPinnedTime p) (pinFileModTime p))
                PinCheckFileNotFound -> return (Nothing, return "The file was moved or deleted.")

pinPinnedTime :: Pin -> IO String
pinPinnedTime p =  return $ "\n      Pinned:        " ++ (formatPinTime p)

pinFileModTime :: Pin -> IO String
pinFileModTime p = fmap ("\n      File Changed:  " ++) $ fmap pinTimeFormat $ getModificationTime $ pinPath p

join :: IO String -> IO String -> IO String
join s1 s2 = (++) <$> s1 <*> s2

cmdQuit = return () --exitSuccess?

{-
    scanning one file
    1. find pins in file
    2. if no pins found -> print message and end
    3. otherwise for each in, display the line, prompt user for alias, and save
-}

-- TODO need to handle pins that already exist
cmdScan :: FilePath -> [ScanArgs] -> IO ()
cmdScan f [] = pinFromFile f
cmdScan f (x:xs) = putStrLn "Scanning args not implemented."

pinFromFile :: FilePath -> IO ()
pinFromFile f = do 
                p <- scanFile f
                -- what if we match the list of found pins against known pins
                case length p of
                    0 -> putStrLn "No pins found."
                    otherwise -> do
                                    putStrLn $ show (length p) ++ " pins found. Enter an alias."
                                    -- here's the current place to short-circut the creation of a pin if it already exists.
                                    -- but, should the user be notified that pins aready exist in the scan file?
                                    --newPin <- promptForAlias f p 
                                    mapM_ (>>= savePin) $ map (promptForAlias f) p

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

cmdHelp = putStrLn "pin is a utility which allows you to put structured metadata (pins) \nin text files."

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

-- TODO watch out! this has been hard-coded to work with the file structure, pinAlias = "lastly"}
cmdRename :: FilePath -> String -> String -> IO ()
cmdRename f old new = updatePinFile f ("\"" ++ old ++ "\",") ("\"" ++ new ++ "\",")

updatePinFile :: FilePath -> String -> String -> IO ()
updatePinFile f old new = do
                        tmp <- copyToTemp f
                        cnt <- getFileContents tmp
                        writeFile f $ unlines $ map (update . words) cnt where
                            update line = intercalate " " $ replace old new line

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs) = case x == old of
                            True -> new : replace old new xs
                            False -> x : replace old new xs

-- Updating hashes is weird for files that are only one line. The file hash and the line hash are the same.
cmdUpdateHashes :: FilePath -> String ->  IO ()
cmdUpdateHashes f p = do 
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


run :: Maybe PinCommand -> IO ()
run (Just CommandList)          = cmdList "pin.pin"
run (Just (CommandScan f a))    = cmdScan f a
run (Just CommandQuit)          = cmdQuit
run (Just CommandHelp)          = cmdHelp
run (Just (CommandShow p))      = cmdShow p
run (Just (CommandDelete p))    = cmdDel p "pin.pin"
run (Just (CommandRename o n))  = cmdRename "pin.pin" o n
run (Just ((CommandUpdate p)))  = cmdUpdateHashes "pin.pin" p
run Nothing                     = cmdQuit

parseArgs :: [String] -> Maybe PinCommand
parseArgs [] = Just CommandQuit
parseArgs (x:xs) = case x of
                    "list" -> Just CommandList
                    "ls"   -> Just CommandList
                    "show" -> Just (CommandShow xs)
                    "scan" -> makeScan xs
                    "help" -> Just CommandHelp
                    "del"  -> makeDel xs
                    "alias"-> makeRename xs
                    "update" -> makeUpdate xs
                    -- TODO need some kind of settings, file types, remove the pins on scan
                    otherwise -> Nothing

-- originally this was going to allow the user to 
-- enter the aliases (alii?) at the prompt.
-- this proved unweildy, so now each pin in the files
-- entered at the prompt are scanned and prompted at
-- each protopin.
-- the second value of the type (xs) are the scan flags
makeScan :: [String] -> Maybe PinCommand
makeScan [] = Nothing
makeScan (x:xs) = Just $ CommandScan x xs

makeDel :: [String] -> Maybe PinCommand
makeDel [] = Nothing
makeDel (x:xs) = Just $ CommandDelete x

makeRename :: [String] -> Maybe PinCommand
makeRename [] = Nothing
makeRename (x:y:xs) = Just $ CommandRename x y

makeUpdate :: [String] -> Maybe PinCommand
makeUpdate [] = Nothing
makeUpdate (x:xs) = Just $ CommandUpdate x

check' :: String -> IO PinCheck
check' p = do
                    pins <- findPin p "pin.pin"
                    let pin = head pins
                    check pin

-- File Checkss
-- 1. In same location.
-- 2. File hash mismatch
-- 3. Line hash mismatch
-- All checks will be performed and reported.
data PinCheck = PinCheckSuccess | PinCheckFileNotFound | PinCheckFileModified | PinCheckLineModified deriving (Show)

-- TODO need a way to update hashes so the user can acknowledge that a file has changed
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

-- Designed to be a short value that contains no spaces.
-- For use mostly in system files. May not be useful for user-visible elements.
timeStamp :: IO String
timeStamp = do
            utcTime <- getCurrentTime
            return (formatTime defaultTimeLocale "%T,%F(%Z)" utcTime)

formatPinTime :: Pin -> String
formatPinTime = pinTimeFormat . pinStoreTime

pinTimeFormat :: UTCTime -> String
pinTimeFormat = (formatTime defaultTimeLocale "%T,%F(%Z)")

-- TODO why can't this be one line?
cleanUpTemps :: FilePath -> IO ()
cleanUpTemps d = do
                    files <- getDirectoryContents d
                    mapM_ delIfTemp files
                        -- FilePath -> IO ()  IO [FilePath]
                        -- IO [FilePath -> IO ()]

delIfTemp :: FilePath -> IO ()            
delIfTemp f = if (take 2 f) == tempPattern then
                removeFile f
              else
                return ()

c = confirm yes no "Do you like babies?"

onYes = putStrLn "You answered yes."
onNo  = putStrLn "You said no."
yes = Confirm onYes "Y"
no =  Confirm onNo "N"

data Confirm = Confirm {
    confirmAction :: IO (),
    confirmResponse :: ConfirmResponse 
    } 

type ConfirmMsg = String
type ConfirmResponse = String

-- this is interesting. the case expession didn't work to define proc, but nested if/then does.
confirm :: Confirm -> Confirm -> ConfirmMsg -> IO ()
confirm yes no msg = 
                     let y = confirmResponse yes 
                         n = confirmResponse no in
                            putStrLn (msg ++ " " ++ y ++ "/" ++ n) >>
                            getLine >>= proc where 
                                    proc res =
                                        if res == (confirmResponse yes) then
                                            confirmAction yes
                                        else
                                            if res == (confirmResponse no) then
                                                confirmAction no
                                            else
                                                confirm yes no msg

tryParse s = let r = parseRes yes no s in
             case r of 
                Nothing -> putStrLn "Nothing"
                Just c -> confirmAction c

parseRes :: Confirm -> Confirm -> String -> Maybe Confirm
parseRes confirmYes confirmNo response =
    if (confirmResponse confirmYes) == response then
        Just confirmYes
    else
        if (confirmResponse confirmNo) == response then
            Just confirmNo
        else
            Nothing
