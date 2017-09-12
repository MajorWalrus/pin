-- main.hs
{-
    Main is the entry point for the application and does command parsing.
-}

module Main
    (
       main
    ) where

import System.Environment (getArgs)
import System.Directory (doesFileExist, getAppUserDataDirectory, getTemporaryDirectory)
import UI
import Util(cleanUpTemps)

main = do
    getTemporaryDirectory >>= cleanUpTemps
    pinFile "pin.pin" >>= ensurePin
    (fmap parseArgs $ getArgs) >>= run

data PinCommand = CommandList | CommandShow {cmdPinAlias :: [String] } | 
    CommandScan { scanPath :: FilePath, scanFlags :: [String] } | CommandHelp | 
    CommandQuit | CommandDelete { delPin :: String} | CommandRename {oldPin :: String, newPin :: String} |
    CommandUpdate { updatePin :: String } | CommandDetail { detailPin :: String } | CommandPath { pathPin :: String, pathPath :: String }
    deriving (Show)

run :: Maybe PinCommand -> IO ()
run (Just CommandList)          = pinFile "pin.pin" >>= cmdList              -- good
run (Just (CommandScan f a))    = pinFile "pin.pin" >>= cmdScan f a -- WHERE DOES pin.pin come from here?
run (Just CommandQuit)          = cmdQuit
run (Just CommandHelp)          = cmdHelp
run (Just (CommandShow p))      = pinFile "pin.pin" >>= cmdShow p           -- good
run (Just (CommandDelete p))    = pinFile "pin.pin" >>= cmdDel p            -- good
run (Just (CommandRename o n))  = pinFile "pin.pin" >>= cmdRename o n       -- good
run (Just ((CommandUpdate p)))  = pinFile "pin.pin" >>= cmdUpdateHashes p   -- good    
run (Just (CommandDetail p))    = pinFile "pin.pin" >>= cmdDetail p         -- good
run (Just (CommandPath p f))    = pinFile "pin.pin" >>= cmdPath p f         -- good
run Nothing                     = cmdQuit                                   -- good

pinFile :: String -> IO FilePath
pinFile f = fmap (++ "\\" ++ f) $ getAppUserDataDirectory "pin"

ensurePin :: String -> IO ()
ensurePin f = do
                fnd <- doesFileExist f
                if fnd then
                    return ()
                else
                    writeFile f ""

parseArgs :: [String] -> Maybe PinCommand
parseArgs [] = Just CommandQuit
parseArgs (x:xs) = case x of
                    "list"      -> Just CommandList
                    "ls"        -> Just CommandList
                    "show"      -> Just (CommandShow xs)
                    "scan"      -> makeScan xs
                    "help"      -> Just CommandHelp
                    "del"       -> makeDel xs
                    "alias"     -> makeRename xs
                    "update"    -> makeUpdate xs
                    "detail"    -> makeDetail xs
                    "path"      -> makePath xs
                    otherwise   -> Nothing

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

makeDetail :: [String] -> Maybe PinCommand
makeDetail [] = Nothing
makeDetail (x:xs) = Just $ CommandDetail x

makePath :: [String] -> Maybe PinCommand
makePath [] = Nothing
makePath (x:[]) = Nothing
makePath (x:y:xs) = Just $ CommandPath x y