module Redsift.Mail where

import Network.SMTPS.Gmail
import System.IO

sendCSVExportMail :: String -> String -> String -> String -> IO ()
sendCSVExportMail user password recipient url =
    let from = user
        pass = password
        to   = [recipient]
        cc   = []
        bcc  = []
        subj = "Your Redsift Export"
        body = "Hi Redsift User, \n\n Here's your exported data URL:\n" ++ url ++ "\n\nThanks,\n\n This email was sent on behalf of Zalora DataScience team"
    in withFile "/dev/null" WriteMode $ \h -> do
        putStrLn $ "Sending to " ++ recipient ++ ":\n" ++ url
        sendGmail h from pass to cc bcc subj body
