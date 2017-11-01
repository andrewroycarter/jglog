module Main where

import Data.List
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Text as Text

main :: IO ()
main = putStrLn =<< fmap makeHTMLForText getContents

makeHTMLForText :: String -> String
makeHTMLForText = join . map makeHTMLForTicket . catMaybes . map ticketNameForJIRALink . getJIRALinks . words . map toLower

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast input = Just (last input)

hasHTTP :: String -> Bool
hasHTTP = isInfixOf "http"

hasJIRA :: String -> Bool
hasJIRA = isInfixOf "jira"

isJIRALink :: String -> Bool
isJIRALink = and . ap [hasJIRA, hasHTTP] . return

getJIRALinks :: [String] -> [String]
getJIRALinks = filter isJIRALink

ticketNameForJIRALink :: String -> Maybe String
ticketNameForJIRALink = fmap Text.unpack . maybeLast . Text.splitOn (Text.pack "/") . Text.pack

makeHTMLForTicket :: String -> String
makeHTMLForTicket ticket = "<p>\n\
\\t<ac:structured-macro ac:macro-id=\"81c485b0-4488-496f-8286-51f393246425\" ac:name=\"jira\" ac:schema-version=\"1\">\n\
\\t<ac:parameter ac:name=\"showSummary\">false</ac:parameter>\n\
\\t<ac:parameter ac:name=\"server\">JIRA</ac:parameter>\n\
\\t<ac:parameter ac:name=\"columns\">key,summary,type,created,updated,due,assignee,reporter,priority,status,resolution</ac:parameter>\n\
\\t<ac:parameter ac:name=\"serverId\">server-id-here</ac:parameter>\n\
\\t<ac:parameter ac:name=\"key\">" ++ ticket ++ "</ac:parameter>\n\
\\t</ac:structured-macro>\n\
\</p>\n"