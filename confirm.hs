-- confirm.hs
{-
    Confirm provides the basic functions to perform UI confirmations of app actions.
-}

module Confirm
    (
        Confirm (..), ConfirmMsg, ConfirmResponse, confirm
    ) where

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