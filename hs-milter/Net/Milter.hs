
module Net.Milter where

import Prelude hiding (catch)
import IO hiding (catch, bracket, try)
import Control.Concurrent
import Control.Exception

import Foreign
import Foreign.StablePtr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Data.Array.IO

import Data.PackedString

import Monad

import Data.IORef

import System
import Directory

import Misc
import Net.Talk

import Text.Printf

import Logger

import Control.Monad.State
import Control.Monad.Reader


type SockAddrString = String

data B a = B a
instance Show (B a) where
 show (B _) = ""

data Cmd
    = Connect String SockAddrString
    | Helo String
    | EnvFrom [String]
    | EnvRcpt [String]
    | Header String String
    | EOH
    | Body (B (IO RawData))
    | EOM
    | Abort
    | Close
 deriving Show

data Result
    = Continue 
    | Reject 	
    | Discard 
    | Accept 
    | Tempfail
 deriving Show




