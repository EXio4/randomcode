{-# LANGUAGE BangPatterns, RankNTypes, GADTs, ViewPatterns #-}

module Main (main) where

import System.Process
import Data.Maybe
import Data.Char
import Data.List
import Control.Applicative
import Control.Monad
import System.Console.Readline

data Command = Play !String
             | RepeatPrevious
             | SetPrompt !String
             | SetLang   !Lang
             | Help
             | DoNothing
             | Quit

data Err = InvalidCommand
         | InvalidLang !String
         | NoPreviousPlay

{- 
Valid languages:
  en-US
  en-GB
  de-DE
  es-ES
  fr-FR
  it-IT
-}
data Lang = EnglishUS
          | EnglishUK
          | German
          | Spanish
          | French
          | Italian
  deriving (Show,Eq,Ord,Enum,Bounded)

data Config = Config {
                lang     :: !Lang        ,
                previous :: Maybe String,
                prompt   :: String
            }

toSetting :: Lang -> String
toSetting EnglishUS = "en-US"
toSetting EnglishUK = "en-GB"
toSetting German    = "de-DE"
toSetting Spanish   = "es-ES"
toSetting French    = "fr-FR"
toSetting Italian   = "it-IT"

showErr :: Err -> String
showErr InvalidCommand  = "Invalid command, try again."
showErr (InvalidLang x) = "Invalid language (" ++ x ++ "), try again."
showErr NoPreviousPlay  = "Err, we haven't played anything (yet)"

parseL :: String -> Either Err Lang
parseL (map toLower -> x)
          | x `elem` ["english"   , "english_us", "us", "en-us"               ] = Right EnglishUS
          | x `elem` [              "english_uk", "uk", "en-uk", "gb", "en-gb"] = Right EnglishUK
          | x `elem` ["german" , "deutsch", "de", "de-de"] = Right German
          | x `elem` ["french", "fr", "fr-fr"] = Right French
          | x `elem` ["it", "italian", "it-it"] = Right Italian
          | x `elem` ["es", "spanish", "es-es"] = Right Spanish
parseL xs = Left (InvalidLang xs)

parseC :: String -> Either Err Command
parseC = p
   where p (':':cmd) | let qs = words cmd
                    = cmd_parses qs
         p "" = Right DoNothing
         p xs = Right (Play xs)
         cmd_parses [x] | x `elem` ["repeat", "r", "previous", "prev", "p"]
                        = Right RepeatPrevious
                        | x `elem` ["quit"  , "q"]
                        = Right Quit
                        | x `elem` ["h", "help"]
                        = Right Help
         cmd_parses (x:y:xs) | x == "set" , y == "prompt" = Right (SetPrompt (unwords xs))
                             | x == "set" , y == "lang"   = SetLang <$> parseL (unwords xs)
         cmd_parses (x:xs)   | x == "setp"                = Right (SetPrompt (unwords xs))
                             | x == "setl"                = SetLang <$> parseL (unwords xs)
         cmd_parses [] = Right RepeatPrevious
         cmd_parses _ = Left InvalidCommand

play :: String -> String -> IO ()
play lang xs = do
    putStrLn $ "Playing '" ++ xs ++ "'"
    callProcess "pico2wave" ["-l", lang,"-w","/tmp/tts.wav",xs]
    callProcess "aplay"     ["/tmp/tts.wav"]
    return ()


loop :: Config -> IO ()
loop cfg = do
     str <- readline (prompt cfg ++ " ")
     maybe (return ()) addHistory str
     case (maybe (Right Quit) id . fmap parseC) str  of
        Left err  -> rec err
        Right cmd -> case cmd of
               Quit -> return ()
               SetPrompt prompt' -> loop (cfg { prompt = prompt' })
               SetLang   lang'   -> loop (cfg { lang   = lang'   })
               RepeatPrevious -> case previous cfg of
                                      Nothing -> rec NoPreviousPlay
                                      Just p  -> pl p >> loop cfg
               Play p -> pl p >> loop (cfg { previous = Just p })
               Help -> help cfg >> loop cfg
               DoNothing -> loop cfg 
  where rec :: Err -> IO ()
        rec err = putStrLn (showErr err) >> loop cfg
        pl = play (toSetting (lang cfg))

data X = S String | L [String]
help :: Config -> IO ()
help cfg = mapM_ pstr
              [S "pico2play REPL"
              ,S "Available commands:"
              ,L [":q/:quit"     , "Quit REPL"]
              ,L [":set"         , "Settings"]
              ,L ["- prompt XYZ" , "Set prompt to 'XYZ'"]
              ,L ["- lang   LNG" , "Set language to 'LNG'"]
              ,L [":repeat/:prev", "Re-play previous string"]
              ,L [":help"        , "This help!"]
              ,S "Shortcuts"
              ,L [":setl", ":set lang"]
              ,L [":setp", ":set prompt"]
              ,S "Everything not starting with : is considered a string to play"
              ,S "[play = TTS]"]
      where pstr (S x) = putStrLn x
            pstr (L x) = putStrLn $ "\t" ++ foldr t "" x
            t xs w | length xs < 8 = xs ++ "\t\t" ++ w
                   | otherwise     = xs ++ "\t"   ++ w
 
     


main = loop $ Config { prompt = "en>", lang = EnglishUS, previous = Nothing }
