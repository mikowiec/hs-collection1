#!/usr/bin/env runhaskell

module Main where

import Directory
import IO
import System
import List
import Monad
import Char


push as ["darcs-sync", name, repo] = do
  let target = "src/" ++ name
  let verbose = "-v" `elem` as
  when verbose $ putStrLn $ "Processing " ++ target
  exists <- doesDirectoryExist target
  let cmd = if "http" `isPrefixOf` repo
       then ["darcs","send","--sign","--repo",target,repo]
       else ["darcs","push","--repo",target,repo]
  when verbose $ putStrLn $ "Executing " ++ show cmd
  system $ concat $ intersperse " " cmd
  when verbose $ putChar '\n'

push verbose ["darcs", target, repo] = do
  when verbose $ putStrLn $ "Skipping " ++ target


main = foreach_pkg push


-- common pkg implementation below.

parse :: String -> [String]
parse = words

filter_comments :: [String] -> [String]
filter_comments = foldr (\a b -> filter (not . a) . b) id
   [null, null . filter isSpace, ("#" `isPrefixOf`), (";" `isPrefixOf`)]

foreach_pkg op = do
    as <- getArgs
    let pkgfile = "/etc/pkg.list"
    let es = map (\d -> (d,d++pkgfile)) [".","..","../.."]
    ds <- filterM (doesFileExist . snd) es
    case ds of
     ((d,f):_) -> do h <- openFile f ReadMode
		     ps <- hGetContents h
		     setCurrentDirectory d
		     mapM_ (op as . parse)
			   (filter_comments $ lines ps)
     _ -> ioError $ userError "Cannot find package list"

