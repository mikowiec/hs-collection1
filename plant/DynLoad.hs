
module DynLoad where

import Directory
import IO
import System
import Pf

ghc = "ghc"
ghc_flags = "-fglasgow-exts -fallow-overlapping-instances"

{-
run_command :: String -> IO (String,String)
run_command cmd = do
    let out_fn = "run_out"
        err_fn = "run_err"
    system (cmd++" > "++out_fn++" 2> "++err_fn)
    out <- readFile out_fn
    err <- readFile err_fn
    out `seq` err `seq` (removeFile out_fn>>removeFile err_fn)
    return (out,err)
-}

type Res = (String,String)

done_test :: String -> String -> String -> IO (Maybe (Either Res Res))
done_test fn out err = do
    b <- doesFileExist fn
    if b then do r <- readFile fn
                 out_res <- readFile out
                 err_res <- readFile err
                 n <- readIO r
                 removeFile fn
                 return (Just ((if n == 0 then Right else Left) (out_res,err_res)))
         else return Nothing

run_bkg_command cmd = do
    run_sys_command system_bkg cmd

run_command cmd = do
    run_sys_command system cmd

run_now m cmd = do
    fn <- m cmd
    Just r <- fn
    return r

--run_sys_command :: (String -> IO ExitCode) -> String -> IO (IO (Maybe (Either Res Res)))
run_sys_command sys cmd = do
    let [tmp,out,err] = map ("tmp/"++) [".done",".out",".err"]
    removeFile tmp `catch` (\_->return ())
    sys (sprintf (""&." >"&." 2>"&." ; echo $? > "&."") cmd out err tmp)
    return (done_test tmp out err)


system_bkg cmd = system ("(" ++ cmd ++ ") &")

libdir = do
    Right (d,_) <- run_now run_command (ghc++" --print-libdir")
    return (head (lines d))







