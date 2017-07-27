-- main.hs
{-
    Main is the entry point for the application and does command parsing.
-}

module Main
    (
       main
    ) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import UI
import Util(cleanUpTemps)

main = do
    cleanUpTemps "."
    ensurePin "pin.pin"
    (fmap parseArgs $ getArgs) >>= run

data PinCommand = CommandList | CommandShow {cmdPinAlias :: [String] } | 
    CommandScan { scanPath :: FilePath, scanFlags :: [String] } | CommandHelp | 
    CommandQuit | CommandDelete { delPin :: String} | CommandRename {oldPin :: String, newPin :: String} |
    CommandUpdate { updatePin :: String } | CommandDetail { detailPin :: String } | CommandPath { pathPin :: String, pathPath :: String }
    deriving (Show)

run :: Maybe PinCommand -> IO ()
run (Just CommandList)          = cmdList "pin.pin"
run (Just (CommandScan f a))    = cmdScan f a
run (Just CommandQuit)          = cmdQuit
run (Just CommandHelp)          = cmdHelp
run (Just (CommandShow p))      = cmdShow p -- WATCH OUT "pin.pin" is buried in the command
run (Just (CommandDelete p))    = cmdDel p "pin.pin"
run (Just (CommandRename o n))  = cmdRename "pin.pin" o n
run (Just ((CommandUpdate p)))  = cmdUpdateHashes "pin.pin" p
run (Just (CommandDetail p))    = cmdDetail "pin.pin" p
run (Just (CommandPath p f))    = cmdPath "pin.pin" p f
run Nothing                     = cmdQuit

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