-- util.hs
{-
    Util exposes general-purpose functions which are used by many parts of the app.
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
    (
        hashString, hashFile, compareUTC, timeStamp, pinTimeFormat, pinTimeFormat', copyToTemp, cleanUpTemps, replace, tempPattern,
        getFileContents
    ) where


import "cryptonite" Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256(..))
import Data.ByteString.Char8 as C8 (pack)
import Data.Time(getCurrentTime, getZonedTime, UTCTime, ZonedTime(..), utcToZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (getDirectoryContents, removeFile)
import System.IO (hClose, openTempFile)

tempPattern = "~_"
timeFmt = "%m/%d/%Y %I:%M %p"

getFileContents :: FilePath -> IO [String]
getFileContents = fmap lines . readFile

hashString :: String -> Digest SHA256
hashString = hash . C8.pack

hashFile :: FilePath -> IO String
hashFile f = fmap (show . hashString) $ readFile f

compareUTC :: IO UTCTime -> IO UTCTime -> IO Bool
compareUTC a b = do 
                    a' <- a
                    b' <- b
                    return $ a' == b'

-- Designed to be a short value that contains no spaces.
-- For use mostly in system files. May not be useful for user-visible elements.
timeStamp :: IO String
timeStamp = do
            utcTime <- getCurrentTime
            return (formatTime defaultTimeLocale timeFmt utcTime)


pinTimeFormat' :: UTCTime -> IO String
pinTimeFormat' u = do
                    t <- getZonedTime
                    let ZonedTime _ tz = t
                    return (formatTime defaultTimeLocale timeFmt $ utcToZonedTime tz u)


pinTimeFormat :: UTCTime -> String
pinTimeFormat = (formatTime defaultTimeLocale timeFmt)


copyToTemp :: FilePath -> IO FilePath
copyToTemp f = do
                (tmpFile, tmpHand) <- openTempFile "." (tempPattern ++ f)
                hClose tmpHand
                cons <- readFile f
                writeFile tmpFile cons
                return tmpFile

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

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs) = case x == old of
                            True -> new : replace old new xs
                            False -> x : replace old new xs