-- pin.hs

{-
    needs hashing of file and line
    last access of pin
    date pinned
    .pin file needs a home
    stack
    quickcheck
    exception (IO) handling
    pre-existing pins
        - check same alias
        - check same file/hash
    warnings of changes to file or line
    new command "check" which looks for changed files, lines, or missing files in all pins
        - use check interally before showing pin, and when listing pins
    list formatting - make the pins display fixed width
-}


import Data.List (findIndices)
import System.IO
import System.Environment
import System.Exit
import System.Directory (doesDirectoryExist, doesFileExist)

--import Options.Applicative
--import System.Console.ANSI as ANSI

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

findPin :: String -> FilePath -> IO [Pin]
--findPin "" _ = IO []
--findPin _ "" = IO []
findPin a f = fmap (filter (matchAlias a)) (readPins $ getFileContents f)

matchAlias :: String -> Pin -> Bool
matchAlias a p = (pinAlias p) == a

testFile = "C://Users//sjdutch//Desktop//aa_more.txt"
testFile2 = "C://Users//sjdutch//Desktop//aa_524.txt"

-- Should a pin be like (alias)flag1,flag2,flag3>

-- commands
-- scan file = reads a file, looks for (> and shows the user the lines
-- scan file should have flags to ignore or replace known pins
-- scan folder = above, but for all files in a folder
-- scan folder recursive = above, but for all sub-folders
-- scan here = scans the current directory use System.Directory.getCurrentDirectory
-- output for the above should be a list of pins with the first 40 chars of the line, the file, the path
-- read = reads a file and shows the user the contents ???? really?
-- list = list all pins [this should have sorting & filtering]
-- list /a = list all pins with more data
--    displaying known pins should check the hash of the file and the line
--    if the pin has moved to a newline, but the hash has not changed, then
--    just update the pin point
-- pin "<alias>" show the pin detail

main = do
    --allpins
    (fmap parseArgs $ getArgs) >>= run
    --cmd <- fmap parseArgs $ getArgs
    --run cmd
    --getProgName >>= print
    --getEnvironment >>= print

---fmap (map putStrLn) $ getArgs

data PinCommand = CommandList | CommandShow {cmdPinAlias :: [String] } | CommandScan { scanPath :: FilePath, scanFlags :: [String] } | CommandHelp | CommandQuit deriving (Show)
type ScanArgs = String

pad :: String -> String
pad "" = ""
pad s = " " ++ s ++ " "

displayPin :: Pin -> String
displayPin p = (pinAlias p) ++ (pad protoPin) ++ (pinPath p)

tests = [test, test2, test3]

displayPins = mapM_ (putStrLn . displayPin) tests

cmdList :: FilePath -> IO ()
cmdList f = (readPins $ getFileContents f) >>= mapM_ (putStrLn . displayPin)

cmdShow :: [String] -> IO ()
cmdShow = mapM_ (cmdShow' "pin.pin")

cmdShow' :: FilePath -> String -> IO ()
cmdShow' f a = do
            pins <- findPin a f
            mapM_ (>>= putStrLn) (map showPin pins)
{-
    The above is the shortest I can make this function. The types don't line up, so I can't figure out how 
    to do it in one line.
        findPin :: String -> FilePath -> IO [Pin]
        map showPin :: [Pin] -> [IO String]

    The second line means, "Call showPin on every pin that was found. Then bind the result to putstring 
    by calling that function for every string and throwing away the resulting list."
-}

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
                case length p of
                    0 -> putStrLn "No pins found."
                    otherwise -> do
                                    putStrLn $ show (length p) ++ " pins found. Enter an alias."
                                    mapM_ (>>= savePin) $ map (promptForAlias f) p

promptForAlias :: FilePath -> Int -> IO Pin
promptForAlias f l = 
    getLineFromFile f l >>=
    (\line -> putStrLn $ "   File: " ++ f ++ "\n   Line: " ++ (stripNewLine line)) >>
    getLine >>= 
    (\alias -> return $ makePin f l alias)

getLineFromFile :: FilePath -> Int -> IO String
getLineFromFile f l = do
                cnts <- readFile f
                return ((lines $ cnts) !! l)

stripNewLine :: String -> String
stripNewLine = filter (/= '\n')

cmdHelp = putStrLn "pin is a utility which allows you to put structured metadata (pins) \nin text files."

run :: Maybe PinCommand -> IO ()
run (Just CommandList)          = cmdList "pin.pin"
run (Just (CommandScan f a))    = cmdScan f a
run (Just CommandQuit)          = cmdQuit
run (Just CommandHelp)          = cmdHelp
run (Just (CommandShow p))      = cmdShow p
run Nothing                     = cmdQuit

parseArgs :: [String] -> Maybe PinCommand
parseArgs [] = Just CommandQuit
parseArgs (x:xs) = case x of
                    "list" -> Just CommandList
                    "ls"   -> Just CommandList
                    "show"  -> Just (CommandShow xs)
                    "scan" -> makeScan xs
                    "help" -> Just CommandHelp
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

-- File Checkss

-- 1. In same location.
checkFile


-- 2. File hash mismatch

-- 3. Line hash mismatch

-- All checks will be performed and reported.

