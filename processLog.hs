-- file: processLog.hs
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.List (groupBy)
import Data.Function (on)

main = do
       log <- readFile "wsjtx_log.adi"
       mapM_ writeLog $ process log
        where groupByCallsign :: [[String]] -> [[[String]]]
              getCallsign :: [String] -> String
              writeLog :: String -> IO ()
              process :: String -> [String]
              getCallsign = drop 20 . head . filter (\s ->"<sta" == take 4 s)
              groupByCallsign ls = map (map snd) grouped
                                 where grouped = groupBy ((==) `on` fst) [(getCallsign xs,xs) |xs <- ls]
              writeLog lns = writeFile (s ++ "_log.adi") lns
                          where s :: String
                                s = getCallsign . words . head . lines $ lns
              process = map unlines . map (map unwords) . groupByCallsign . map words . tail . lines
