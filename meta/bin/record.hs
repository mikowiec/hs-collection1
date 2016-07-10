#!/usr/bin/env runhaskell

module Main where

import Directory
import IO
import System
import List
import Monad
import Char


record as [darcs, name, repo] | "darcs" `isPrefixOf` darcs = do
  let target = "src/" ++ name
  exists <- doesDirectoryExist target
  let cmd = ["darcs","whatsnew","--repo",target, "-sl"]
  when ("src/ext" `isNotPrefixOf` target && exists) $ do
      putStrLn $ "[ " ++ name ++ " ]"
      ec <- system $ concat $ intersperse " " cmd
      putChar '\n'
      case ec of
       ExitFailure 1 -> return ()
       _ -> do
        shell <- getEnv "SHELL"
        ec <- withCurrentDirectory target $ system shell
        when (ec /= ExitSuccess) (exitWith (ExitFailure 1))
        return ()

isNotPrefixOf p s = not (p `isPrefixOf` s)

withCurrentDirectory d m = bracket_
  getCurrentDirectory setCurrentDirectory (setCurrentDirectory d >> m) 

main = foreach_pkg record


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

