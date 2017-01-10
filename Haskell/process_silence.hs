{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Maybe
import Text.Read
import Control.Exception
import System.IO
import System.Exit
import System.IO.Error
import System.Environment
import System.Process

data SIL = SilenceStart !Double
         | SilenceEnd   !Double !Double {- time and duration -}
    deriving (Show,Eq)

getIf :: String -> String -> Maybe String
getIf xs prefix = let (as,bs) = splitAt (length prefix) xs
                  in if as == prefix
                     then Just bs
                     else Nothing

getSIL :: String -> [SIL]
getSIL file = mapMaybe f . map (drop 33) . filter (\x -> "[sil" == take 4 x) . map (reverse . takeWhile (/= '\r') . reverse) . lines $ file
    where f xs | Just start <- readMaybe =<< getIf xs "silence_start: "
               = Just $ SilenceStart start
               | (as,bs) <- fmap (drop 2) . span (/= '|') $ xs
               , Just end <- readMaybe =<< getIf as "silence_end: "
               , Just duration <- readMaybe =<< getIf bs "silence_duration: "
               = Just $ SilenceEnd end duration
          f _ = Nothing

data SilenceInfo = SilenceBetween !Double !Double !Double
    deriving (Show,Eq)

silInf :: [SIL] -> [SilenceInfo]
silInf (SilenceStart start : SilenceEnd end duration : xs) = SilenceBetween start end duration : silInf xs
silInf (_ : xs) = silInf xs
silInf [] = []

diffBetweenSilences :: [SilenceInfo] -> [(Double, Double)]
diffBetweenSilences ((c@(SilenceBetween s1 e1 d1)) : xs@(SilenceBetween s2 e2 d2 : _)) = (s1,s2-e1) : diffBetweenSilences xs
diffBetweenSilences [SilenceBetween s e d] = [(s, 999)]
diffBetweenSilences [] = []

data Content = Content { startTime :: !Double , endTime :: !Double }
    deriving (Show,Eq)

tracks :: [SIL] -> [Content]
tracks = go 0 where
    go !currStart [] = []
    go !currStart (SilenceStart s : xs) = Content currStart s : go s xs
    go !currStart (SilenceEnd e _ : xs) = go currStart xs


tracks_smart :: [SIL] -> [Content]
tracks_smart = go 0 Nothing where
    go !currStart Nothing  [] = []
    go !currStart (Just v) [] = [v]
    go !currStart Nothing (SilenceStart s : xs) = go s (Just (Content currStart s)) xs
    go !currStart (Just (Content start end)) (SilenceEnd silence_end _ : xs) = 
            let new_end = (end + silence_end) / 2
            in  Content start new_end : go new_end Nothing xs
    go !currStart (Just _) (SilenceStart _ : xs) = error "Shouldn't happen" {- ?? -}
    go !currStart Nothing (SilenceEnd _ _ : xs) = error "Shouldn't happen" {- ?? -}

duration :: Content -> Double
duration c = endTime c - startTime c

joinIfSmaller :: Double -> [Content] -> [Content]
joinIfSmaller n = go . map shouldWe where
    shouldWe cnt = (duration cnt <= n, cnt)
    go ((cnt, Content s1 e1): (cnt', Content s2 e2) : xs) | cnt = go ((cnt', Content s1 e2) : xs)
    go ((_,x) : xs) = x : go xs
    go [] = []

process :: FilePath -> IO ()
process file = do
    putStr "Gathering silence info ... "
    hFlush stdout
    (exc, _, content) <- readCreateProcessWithExitCode (proc "ffmpeg"
                                                          ["-loglevel", "info"
                                                          ,"-i", file
                                                          ,"-af", "silencedetect=noise=-45dB:d=0.5"
                                                          ,"-f", "null"
                                                          ,"-"]) ""
    case exc of
        ExitFailure n -> do
             putStrLn $ "FAILED [" ++ show n ++ "]"
             putStrLn content
        ExitSuccess -> do
            putStrLn "OK"
            let cnts = joinIfSmaller 27.27 (tracks_smart (getSIL content))
            mapM_ p (zip [0..] cnts)
    where p (out,cnt) = do
                    let cmd = "ffmpeg"
                    let params = ["-ss" ,s (truncate (startTime cnt)),
                                  "-t"  ,s (ceiling  (duration  cnt)),
                                  "-i"  ,file,
                                  file' out]
                    putStr $ "Handling [" ++ show (truncate $ startTime cnt) ++ " to " ++ show (ceiling $ endTime cnt) ++ " (dur: " ++ show (ceiling $ duration cnt) ++ ")]" ++ "\tN/" ++ s2 out ++ " "
                    hFlush stdout
                    (_, _, Just std_err, handle) <- createProcess (proc cmd params){ std_err = CreatePipe }
                    x <- waitForProcess handle
                    case x of
                         ExitFailure n -> do
                             putStrLn $ "ERROR [" ++ show n ++ "]"
                             putStrLn =<< hGetContents std_err
                         ExitSuccess -> putStrLn "OK"
                    hClose std_err
          s t = let (a,s) = t `divMod` 60
                    (h,m) = a `divMod` 60
                in s2 h ++ ":" ++ s2 m ++ ":" ++ s2 s
          s2 x | x < 10    = "0" ++ show x
               | otherwise = show x
          file' x = let (as,bs) = span (/= '.') file
                    in as ++ "/" ++ s2 x ++ bs

main :: IO ()
main = do
    [file] <- getArgs
    process file
