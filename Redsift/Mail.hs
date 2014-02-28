module Redsift.Mail where

import Network.SMTPS.Gmail
import System.IO

import Redsift.Config


sendCSVExportMail :: GmailConfig -> String -> String -> IO ()
sendCSVExportMail config recipient url =
    let subj = "Your Redsift Export"
        body = "Hi Redsift User, \n\n Here's your exported data URL:\n" ++ url ++ "\n\nThanks,\n\n This email was sent on behalf of Zalora DataScience team"
    in do
        hPutStrLn stderr $ "Sending to " ++ recipient ++ ":\n" ++ url
        sendMail config recipient subj body

sendMail :: GmailConfig -> String -> String -> String -> IO ()
sendMail (GmailConfig from password) recipient subj body =
    let to   = [recipient]
        cc   = []
        bcc  = []
    in sendGmail stderr from password to cc bcc subj body
