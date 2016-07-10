
module Net.MilterTest where

import Net.MilterProtocol
import Net.MilterProtocolIO
import Net.MilterTalk

import Control.Exception

import Directory

import Network
import Network.BSD

import System.IO

socket = "/var/lib/milter/filter.sock"

main = do
    run_milter "buuuh" socket undefined



