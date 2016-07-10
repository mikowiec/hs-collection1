
module Main where

import Net.Sendmail


main :: IO ()
main = do
    sendmail [server "localhost", port 25, from "random@zarquon.se",
              to "Peter Strand <strand@zarquon.se>", subject "Sendmail.hs test",
              header "X-Haskell" "Yar!"]
             "yay!"
    return ()
    
