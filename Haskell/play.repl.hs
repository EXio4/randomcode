module Main (main) where

import System.Process
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Console.Readline

data Command = Play !String
             | RepeatPrevious
             | SetPrompt !String
             | SetLang   !String
             | DoNothing
             | Quit

data Err = InvalidCommand
         | NoPreviousPlay

data Config = Config {
                lang     :: String      ,
                previous :: Maybe String,
                prompt   :: String
            }

showErr :: Err -> String
showErr InvalidCommand = "Invalid command, try again."
showErr NoPreviousPlay = "Err, we haven't played anything (yet)"

parseC :: String -> Either Err Command
parseC = p
   where p (':':cmd) | let qs = words cmd
                     = maybe (Left InvalidCommand) id (fmap Right (cmd_parses qs))
         p "" = Right DoNothing
         p xs = Right (Play xs)
         cmd_parses [x] | x `elem` ["repeat", "r", "previous", "prev", "p"]
                        = Just RepeatPrevious
                        | x `elem` ["quit"  , "q"]
                        = Just Quit
         cmd_parses (x:y:xs) | x == "set" , y == "prompt" = Just (SetPrompt (unwords xs))
                             | x == "set" , y == "lang"   = Just (SetLang   (unwords xs))
         cmd_parses (x:xs)   | x == "setp"                = Just (SetPrompt (unwords xs))
                             | x == "setl"                = Just (SetLang   (unwords xs))
         cmd_parses [] = Just RepeatPrevious
         cmd_parses _ = Nothing

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
                                      Just p  -> play (lang cfg) p >> loop cfg
               Play p -> play (lang cfg) p >> loop (cfg { previous = Just p })
               DoNothing -> loop cfg 
  where rec :: Err -> IO ()
        rec err = putStrLn (showErr err) >> loop cfg
 
     


main = loop $ Config { prompt = "en>", lang = "en-US", previous = Nothing }
