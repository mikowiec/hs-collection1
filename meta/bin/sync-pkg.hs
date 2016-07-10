#!/usr/bin/env runhaskell

module Main where

import Directory
import IO
import System
import List
import Monad
import Char
import Time


main = do
    as <- getArgs
    let verbose   = "-v" `elem` as
        dry       = "-d" `elem` as
    let pkgfile = "/etc/local"
    let es = map (\d -> (d,d++pkgfile)) [".","..","../.."]
    ds <- filterM (doesFileExist . snd) es
    case ds of
     ((d,f):_) -> do 
        h <- openFile f ReadMode
        cs <- hGetAllContents h
        hClose h
        let pkg_specs = (map parse . filter_comments . lines) cs
        let no_dots = filter (not . (`elem` [".",".."]))
        rs <- no_dots `liftM` getDirectoryContents (d++"/src")
        let pkg_names = map (\[_,t,_] -> t) pkg_specs
        rs' <- filterM (\d' -> doesDirectoryExist (d++"/src/"++d'++"/_darcs")) rs
        let new = sort $ filter (not . (`elem` pkg_names)) rs'
        new_spec <- mkSpec d new
        when (verbose || dry) $ do
          putStr $ unlines $ map formatPkgSpec new_spec
        when (not dry) $ do
          updatePkgFile f new_spec

mkSpec d xs = mapM f xs
 where f x = do
        repo <- getRemoteRepo d x
        let darcs = if "ext" `isPrefixOf` x then "darcs" else "darcs-sync"
        return $ [darcs,x,maybe "http://" id repo]

getRemoteRepo d r = do
  let def_repo = d ++ "/src/" ++ r ++ "/_darcs/prefs/defaultrepo"
  (Just . takeWhile isPrint) `liftM` readFile def_repo
 `catch` (\_->return Nothing)

formatPkgSpec [d,t,r] =
  d ++ replicate (10 - length d) ' ' ++ " " ++
  t ++ replicate (20 - length t) ' ' ++ " " ++
  r

updatePkgFile f new = do
  h <- openFile f AppendMode
  date <- toCalendarTime =<< getClockTime
  hPutStrLn h ""
  hPutStrLn h $ "# Added by sync-pkg, " ++ calendarTimeToString date
  hPutStr h $ unlines $ map formatPkgSpec new
  hClose h

hGetAllContents h = do
  s <- hGetContents h
  length s `seq` return s


-- common pkg implementation below.

parse :: String -> [String]
parse = words

filter_comments :: [String] -> [String]
filter_comments = foldr (\a b -> filter (not . a) . b) id
   [null, null . filter isSpace, ("#" `isPrefixOf`), (";" `isPrefixOf`)]

