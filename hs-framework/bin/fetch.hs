#!/usr/bin/env runhaskell

module Main where

import Directory
import IO
import System
import List
import Monad
import Char


fetch as [darcs, name, repo] | "darcs" `isPrefixOf` darcs = do
  let target = "src/" ++ name
  let verbose = "-v" `elem` as
  let update  = "-u" `elem` as
  when verbose $ putStrLn $ "Processing " ++ name
  exists <- doesDirectoryExist target
  let cmd = if exists
       then ["darcs","pull","--repo",target,repo]
       else ["darcs","get",repo,target]
  when (not exists || update) $ do
      when verbose $ putStrLn $ "Executing " ++ show cmd
      system $ concat $ intersperse " " cmd
      when verbose $ putChar '\n'

is_selected [] _ = True
is_selected cs s@[_, t, _] = mod `elem` cs
 where mod = reverse $ takeWhile (/='/') (reverse t)
is_selected cs _ = False


main = foreach_pkg fetch


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

