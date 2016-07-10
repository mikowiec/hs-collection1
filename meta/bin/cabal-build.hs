#!/usr/bin/env runhaskell

import List
import Directory
import Distribution.PackageDescription
import Distribution.Version
import Text.Printf
import System
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.Query
import Control.Monad.State
import Monad
import qualified Data.Map as Map
import Data.IORef
import Control.Exception

pkgToPath p = printf "%s/%s.cabal" p p

type Deps = Gr String String

idSupply = newIORef (0,Map.empty)

nameToId :: IORef (Int,Map.Map String Int) -> String -> IO Int
nameToId ref n = do
  (id,t) <- readIORef ref
  let ins = do let id' = succ id
               writeIORef ref (id', Map.insert n id' t)
               return id'
  maybe ins return (Map.lookup n t)

depName (Dependency n _) = n

storeNode ref d = do
  i <- lift $ nameToId ref d
  modify (\(ns,es) -> ((i,d):ns, es))
storeEdge ref d d' = do
  i <- lift $ nameToId ref d
  i' <- lift $ nameToId ref d'
  modify (\(ns,es) -> (ns, (i,i',""):es))


buildDeps :: String -> IO Deps
buildDeps start = do
  ref <- idSupply
  let 
      build [] = return ()
      build (d:ds) = do
       storeNode ref d
       let fp = pkgToPath d
       b <- lift $ doesFileExist fp
       when b $ do
        pkg <- lift $ try $ readPackageDescription (pkgToPath d)
        case pkg of
         Left _ -> return ()
         Right pkg -> do          
          let ds = map depName $ buildDepends pkg
          build ds
          mapM_ (\d' -> storeEdge ref d d') ds
       build ds
  (ns,es) <- execStateT (build [start]) ([] :: [LNode String],[] :: [LEdge String])
  return $ mkGraph (nub ns) (nub es)

main = do
  (a:_) <- getArgs
  g <- buildDeps a
  writeFile (a++".dot") $ graphviz g ("\"Dependencies for " ++ a ++ "\"") (10,10) (1,1) Portrait
  let pkgs = reverse (topsort' g)
  (`mapM_` pkgs) $ \p -> do
    b <- doesDirectoryExist p
    cwd <- getCurrentDirectory
    when b $ do
      printf "\nBuilding %s\n" p
      setCurrentDirectory p
      ExitSuccess <- system "runhaskell Setup.hs configure --user --prefix=$HOME"
      ExitSuccess <- system "runhaskell Setup.hs build"
      ExitSuccess <- system "runhaskell Setup.hs install --user"
      setCurrentDirectory cwd
    

  

