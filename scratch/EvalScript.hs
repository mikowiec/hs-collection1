
module EvalScript where

import System.Plugins
import List


fail_msg err = fail $ filter (/='\r') $ concat (intersperse "\n" err)

eval :: String -> String -> IO (a, IO ())
eval buffer symbol = do
  let file = "Scene.hs"
  writeFile file buffer
  ms <- makeAll file ["-fglasgow-exts"]
  case ms of
    MakeFailure err -> fail_msg err
    MakeSuccess mc obj -> do
      status <- load_ obj ["."] symbol
      case status of
        LoadFailure msg -> fail_msg msg
        LoadSuccess mod f -> do
          return (f,unload mod)




