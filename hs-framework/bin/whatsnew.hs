#!/usr/bin/env runhaskell

module Main where

import Directory
import IO
import System
import List
import Monad
import Char


whatsnew [darcs, name, repo] | "darcs" `isPrefixOf` darcs = do
  let target = "src/" ++ name
  exists <- doesDirectoryExist target
  let cmd = ["darcs","whatsnew","--repo",target, "-sl"]
  when (exists) $ do
      putStrLn $ "[ " ++ name ++ " ]"
      system $ concat $ intersperse " " cmd
      putChar '\n'

main = foreach_pkg whatsnew


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
		     mapM_ (op . parse)
			   (filter_comments $ lines ps)
     _ -> ioError $ userError "Cannot find package list"

