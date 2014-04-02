{-# LANGUAGE ScopedTypeVariables #-}
module Network.HWifi where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HWifi
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  portable
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- A simple module to deal with wifi connections.
-- At the moment, only election of the wifi with the most powerful signal and autoconnect policy.
--
-- Use: runhaskell Network/HWifi.hs
-----------------------------------------------------------------------------

import Data.Functor ((<$>))
import Data.List (intersect, sort)
import Control.Monad.Writer hiding (mapM_)
import Prelude hiding (elem)
import Control.Arrow ((***), second)
import Network.Utils (clean
                     ,logMsg
                     ,run
                     ,catchIO
                     ,Command(Scan)
                     ,Command(Connect)
                     ,conCmd
                     ,scanCmd
                     ,knownCmd
                     ,connect)
import Control.Exception

type WifiMonad w a = WriterT w IO a

type SSID  = String
type Signal= String
type Wifi  = (SSID, Signal)
type Log   = String

runWifiMonad :: WifiMonad w a -> IO (a, w)
runWifiMonad  = runWriterT

-- | Slice a string "'wifi':signal" in a tuple ("wifi", "signal")
parse :: String -> Wifi
parse = wifiDetails
  where wifiDetails = (clean '\'' *** tail) .  break (== ':')

available:: Command -> WifiMonad [Log][SSID]
available (Connect _) = tell ["Irrelevant Command Connect for available function"] >> return []
available (Scan cmd)  = runWithLog allWifis logAll
  where allWifis = (map (fst . second sort) . map parse) <$> run cmd
        logAll = logMsg ("Scanned wifi: \n") ("- "++)


-- | List the current wifi the computer can connect to
alreadyUsed :: Command -> WifiMonad [Log][SSID]
alreadyUsed (Connect _) = tell ["Irrelevant Command Connect for alreadyUsed function"] >> return []
alreadyUsed (Scan cmd)  = runWithLog (run cmd) logKnown
  where logKnown = logMsg ("\n Auto-connect wifi: \n") ("- "++)

-- | Runs a computation and logs f on the computation results
runWithLog :: (Monoid b) => IO a -> (a -> b) -> WifiMonad b a
runWithLog comp f = do
  result <- liftIO comp
  tell $ f result
  return result

-- | Elect wifi according to signal's power joined to a list of auto connect ones
-- | This function throw an exception if you give an empty`wifis` parameter
elect ::[SSID] -> [SSID] -> SSID
elect wifis = head . intersect wifis

-- | Safe version of `elect` that runs in `IO` monad
safeElect ::[SSID] -> [SSID] -> IO SSID
safeElect wifis = (`catchIO` []) . evaluate . head . intersect wifis

-- | Run the connection to a wifi
safeConnect :: String -> IO [String]
safeConnect = (`catchIO` []) . run . connect conCmd

-- | Run the available wifis computation
runAvailable :: WifiMonad [Log] [SSID]
runAvailable = available scanCmd

-- | Run the already used wifis computation
runAlreadyUsed :: WifiMonad [Log] [SSID]
runAlreadyUsed = alreadyUsed knownCmd
