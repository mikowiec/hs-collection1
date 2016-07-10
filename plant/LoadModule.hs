
module LoadModule where

import Monad
import CString
import Foreign
import GHC.Ptr
import GHC.Exts
import Pf
import System
import List
import Directory
import Misc

import DynLoad

foreign import ccall "Linker.h initLinker" initLinker :: IO ()
foreign import ccall "Linker.h loadObj" loadObj  :: CString -> IO Int
foreign import ccall "Linker.h unloadObj" unloadObj  :: CString -> IO Int
foreign import ccall "Linker.h resolveObjs" resolveObjs  :: IO ()
foreign import ccall "Linker.h lookupSymbol" lookupSymbol  :: CString -> IO (Ptr a)

untilM _ _ [] = fail "never satisfied, are you?"
untilM p f (x:xs) = do
    r <- f x
    if p r then return (x,r)
     else untilM p f xs

load_paths = ["./", "tmp/", "/usr/local/ghc-5.04.1/lib/ghc-5.04.1/", "/usr/lib/ghc-5.04/", "c:/ghc/ghc-5.04/"]

load_obj :: String -> IO (Maybe Bool)
load_obj mod = do
    let ts = map (++mod++".o") load_paths
    (r,_) <- untilM (==1) (`withCString` loadObj) ts
--    printf ("loading "&"\n") r
    return (Just True)
 `catch` (\_ -> return (Just False))

unload_obj mod = do
    let ts = map (++mod++".o") load_paths
    (r,_) <- untilM (==1) (`withCString` unloadObj) ts
    printf ("unloading "&"\n") mod
    return True
 `catch` (\_ -> return False)

real_compile rc mod = do
--    printf ("compiling "&."\n") mod
    rc (sprintf ("ghc --make -fallow-overlapping-instances -fglasgow-exts "&.".hs -package lang -package data -package c2hs -package gtkhs") mod)

init :: IO [IO (String, Maybe (IO (Maybe (Either Res Res))))]
init = init' run_bkg_command
init_fg = do 
    xs <- init' run_command
    sequence_ xs

init' rc = do
    initLinker
    return (map (\m -> ((,) ("compiling " ++ m) . Just) $^ real_compile rc m) ["EngineInst2","EngineInst1"] ++
            map (liftM (\_->("loading", Nothing)) . load_obj)
                     (["HSbase", "HSbase_cbits",
                       "HShaskell98",
                       "HSlang", "HSlang_cbits",
                       "HSconcurrent",
                       "HSutil", "HSutil_cbits",
                       "HSposix", "HSposix_cbits",
                       "HSdata",
                       "HSreadline",
                       "HSunix", "HSunix_cbits",
                       "MonadLib", "Types", "Misc", "LinAlg", "Pf",
                       "ArrayLib", "PlantTypes", "ApproxTree",
                       "SpaceWeight", "GeomRep", "Property", "Environment",
                       "SimEngine", "DeepSeq"]) ++
        [resolveObjs >> return ("resolving symbols", Nothing)])


compile' rc org mod = do
    cnt <- readFile (org++".hs")
    let hs  = "tmp/tmp.hs"
        obj = "tmp/"++mod++".o"
        cnt' = tail $ snd $ break (isPrefixOf "module ") $ lines cnt
    writeFile hs (unlines (("module "++mod++ " where\n") : cnt'))
--    printf ("compiling "&." to "&."\n") hs obj
    rc (sprintf ("ghc -fallow-overlapping-instances -fglasgow-exts -no-recomp -c "&." -o "&." -package data -package c2hs -package gtkhs") hs obj)

compile :: String -> String -> IO (IO (Maybe (Either Res Res)))
compile = compile' run_bkg_command
compile_fg m1 m2 = do
    m <- compile' run_command m1 m2
    m

load_sym :: t -> String -> IO a
load_sym _ sym = do
    resolveObjs
    ptr@(Ptr addr) <- withCString sym lookupSymbol
{-
    if ptr == nullPtr then
          printf ("symbol "&" not found\n") sym
     else printf ("symbol "&" found at: "&"\n") sym ptr
-}
    case addrToHValue# addr of
     (# val #) -> return val



