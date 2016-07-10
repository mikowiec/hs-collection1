
module Main where

import Seed2
import Simul2

import LoadModule as LM

import qualified Simul2
import Random
import System
import Directory
import SimEngine

wait_for_file fn = do
    b <- doesFileExist fn
    if b then return () else system "sleep 1" >> wait_for_file fn

run_n_wait [] = return ()
run_n_wait (x:xs) = do
    r <- x
    case r of
     Just fn -> wait_for_file fn >> run_n_wait xs
     Nothing -> run_n_wait xs

main = do
    xs <- LM.init

    run_n_wait (xs ++ [LM.compile "Plants2" "Plants21"])

    print "init done, loading"

    mk_sim <- LM.load "Plants21" "Plants21_mkzuplants_closure" :: IO (IO [(String, SimEngine)])
    (_,eng):_ <- mk_sim

    let ss = iterate se_simulate_step eng
    mapM_ (print . simrep_show) (take 20 ss)

{-
    mk_sim <- LM.load "Plants21" "Plants21_mkzuplants_closure"

    sims <- mk_sim :: IO [(String, SimEngine)]

    print (map fst sims)
    let ss = snd (head sims)
    mapM_ print (take 10 $ map simrep_show (iterate se_simulate_step ss))
-}

    return ()

