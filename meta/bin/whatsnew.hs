#!/usr/bin/env runhaskell

module Main where

import Directory
import IO
import System
import List
import Monad
import Char


whatsnew as [darcs, name, repo] | "darcs" `isPrefixOf` darcs = do
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
    let select = "/etc/select"
    let es = map (\d -> (d,d++select)) [".","..","../.."]
    ds <- filterM (doesFileExist . snd) es
    case ds of
     ((d,f):_) -> do
      h <- openFile f ReadMode
      ps <- hGetContents h
      mapM_ (\f -> process_pkgs as d f) (lines ps)
      hClose h
     _ -> ioError $ userError "Cannot find package selection file"
  where
    process_pkgs as d f | (not ("#" `isPrefixOf` f)) = do
      h <- openFile (d++"/etc/"++f) ReadMode
      ps <- hGetContents h
      setCurrentDirectory d
      mapM_ (op as . parse)
            (filter_comments $ lines ps)
      hClose h
    process_pkgs _ _ _ = return ()

