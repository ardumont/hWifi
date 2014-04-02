module Main
       (main,
        availableWifis,
        alreadyUsedWifis,
        electedWifi) where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Main
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  portable
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- A module to deal with wifi connections.
-- At the moment, only election of the wifi with the most powerful signal and autoconnect policy.
--
-- Use: cabal run
-----------------------------------------------------------------------------

import Control.Monad (join)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Network.HWifi (runWifiMonad,
                      safeConnect,
                      safeElect,
                      knownCmd,
                      scanCmd,
                      alreadyUsed,
                      available,
                      SSID)

-- | Scan the wifi, compute the list of autoconnect wifis, connect to one
-- (if multiple possible, the one with the most powerful signal is elected)
availableWifisWithLogs :: IO ([SSID], [String])
availableWifisWithLogs =  runWifiMonad $ available scanCmd

-- | Compute the available wifi list through a scanning
availableWifis :: IO([SSID])
availableWifis = fst <$> availableWifisWithLogs

-- | Compute the available auto-connect wifi list and formatted logs
alreadyUsedWifisWithLogs :: IO ([SSID], [String])
alreadyUsedWifisWithLogs = runWifiMonad $ alreadyUsed knownCmd

-- | Compute the available auto-connect wifi list on the current machine
alreadyUsedWifis :: IO([SSID])
alreadyUsedWifis = fst <$> alreadyUsedWifisWithLogs

-- | Compute the wifi to connect to.
-- The actual election policy is to elect the most powerful wifi based
-- on available and known auto-connect wifi.
electedWifi :: IO SSID
electedWifi = join $ safeElect <$> alreadyUsedWifis <*> availableWifis

-- | Log function
logAll:: [String]-> IO ()
logAll = mapM_ putStrLn

-- | Do the actual connection to an available and known wifi
main :: IO ()
main = do
  (allWifis, msg1)   <- availableWifisWithLogs
  logAll msg1
  (knownWifis, msg2) <- alreadyUsedWifisWithLogs
  logAll msg2
  let elected = (safeElect knownWifis allWifis)
  _ <- join $ safeConnect <$> elected
  elected >>= putStrLn . ("\n Elected Wifi: "++)
