module Network.Utils where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Utils
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  portable
-- Dependency  :
--
-- Utility functions module
--
-----------------------------------------------------------------------------

import System.Process (readProcess)
import Data.List (delete, isPrefixOf)
import Control.Monad.Error (ErrorT, Error, runErrorT, noMsg, strMsg, MonadIO, liftIO)
import Control.Exception
import System.IO

data CommandError = EmptyCommand | InvalidCommand | OtherError String deriving (Show, Eq)
instance Error(CommandError) where
  noMsg = OtherError "Some problem occured during command execution"
  strMsg = OtherError

type ProcessMonad = ErrorT CommandError IO

runProcessMonad:: ProcessMonad a -> IO (Either CommandError a)
runProcessMonad = runErrorT

-- | Run a command and displays the output in list of strings
run :: String -> IO [String]
run []      = return []
run command = (readProcess comm args [] >>= return . lines) `catchIO` []
  where (comm:args) = words command

-- | Utility function to trim the ' in a string
clean :: Char -> String -> String
clean c cs = if isPrefixOf [c] cs then sanitize cs else cs
  where sanitize = delChar . reverse . delChar . reverse
        delChar  = delete c

logMsg :: String -> (String -> String) -> [String] -> [String]
logMsg prefix f = (prefix :) . map f

-- | executes a given `IO a` action, catches and print to stderr any thrown
-- | exception, then return a defValue and continue execution
catchIO :: MonadIO m => IO a -> a -> m a
catchIO ma defValue = liftIO (ma `catch` \(SomeException e) ->
                      hPrint stderr e >> hFlush stderr >> return defValue)

data Command = Scan{ scan :: String} | Connect {connect :: String -> String}

instance Show Command where
  show (Scan _) = "Scanning for finding some Wifi"
  show (Connect _) = "Connecting to an elected Wifi..."


-- | Command to scan the current wifi
scanCmd :: Command
scanCmd = Scan "nmcli --terse --fields ssid,signal dev wifi"

-- | Command to list the wifi the computer can currently auto connect to
knownCmd :: Command
knownCmd = Scan "nmcli --terse --fields name con list"

-- | Given a wifi, execute the command to connect to a wifi (need super power :)
conCmd :: Command
conCmd = Connect ("sudo nmcli con up id " ++)
