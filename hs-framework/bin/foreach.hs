#!/usr/bin/env runhaskell

module Main where

import Directory
import IO
import System
import List
import Monad
import Char

shell as [darcs, name, repo] | "darcs" `isPrefixOf` darcs = do
  let target = "src/" ++ name
  exists <- doesDirectoryExist target
  let local_ok = ("-l" `elem` as) `forbids` ("src/ext" `isPrefixOf` target)
  let ext_ok   = ("-e" `elem` as) `implies` ("src/ext" `isPrefixOf` target)
  when (local_ok && ext_ok && exists) $ do
    shell <- getEnv "SHELL"
    ec <- withCurrentDirectory target $ system shell
    when (ec /= ExitSuccess) (exitWith (ExitFailure 1))
    return ()

withCurrentDirectory d m = bracket_
  getCurrentDirectory setCurrentDirectory (setCurrentDirectory d >> m) 


forbids cond target = cond `implies` not target
implies cond target = not cond || target

main = foreach_pkg shell


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

