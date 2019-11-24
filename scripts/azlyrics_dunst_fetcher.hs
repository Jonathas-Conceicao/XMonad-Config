#!/bin/env runhaskell
import qualified System.Environment as Env
import qualified System.Process as Proc
import qualified Data.Char as CharUtil

stack_tag :: String
stack_tag = tag_prefix ++ "azlyrics"
  where tag_prefix = "string:x-dunst-stack-tag:"

main :: IO ()
main = do
   args <- Env.getArgs
   case args of
     ["Spotify", summary, body, _icon, urgency] -> do
       response <- Proc.readProcess
         "dunstify"
         [ "-a", "AzLyrics"
         , "-h", stack_tag
         , "-u", urgency
         , "-A", "azlyrics,default"
         , summary, body
         ] []
       case init response of
         "azlyrics" -> notify_lyrics body summary
         _ -> return ()
     _ -> return ()

notify_lyrics :: String -> String -> IO ()
notify_lyrics artist song = do
  Proc.callProcess
        "dunstify"
        [ "-a", "AzLyrics"
        , "-h", stack_tag
        , "-t", "0"
        , "-u", "LOW"
        , artist ++ " - " ++ song, "Fetching..."
        ]
  lyrics <- Proc.readProcess "azlyrics" [fixArtist artist, fixSong song] []
  notify_blocks $ stanza lyrics
  where
    notify_blocks :: [String] -> IO ()
    notify_blocks [] = return ()
    notify_blocks (stanza: ss) = do
      rep <- Proc.readProcess
        "dunstify"
        [ "-a", "AzLyrics"
        , "-h", stack_tag
        , "-t", "0"
        , "-u", "LOW"
        , "-A", "continue,default"
        , artist ++ " - " ++ song, stanza
        ] []
      case init rep of
        "continue" -> notify_blocks ss
        _ -> return ()

stanza :: String -> [String]
stanza s = map unlines $ stanzaAux (lines s) []
  where
    stanzaAux :: [String] -> [String] -> [[String]]
    stanzaAux [] ss = [ss]
    stanzaAux ("": ls) ss = ss:(stanzaAux ls [])
    stanzaAux (l:  ls) ss = stanzaAux ls (ss++[l])

-- temporary workarround

fixArtist :: String -> String
fixArtist = map CharUtil.toLower . filter (/= ' ') . takeWhile ((||) <$> CharUtil.isAlphaNum <*> (' '==))

fixSong :: String -> String
fixSong = map CharUtil.toLower . filter CharUtil.isAlphaNum
