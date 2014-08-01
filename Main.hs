module Main (main
            , availableWifis
            , alreadyUsedWifis
            , electedWifi)
       where

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
import Control.Monad(join)
import Data.Functor((<$>))
import Control.Applicative((<*>))
import Network.Utils(run)
import Network.Nmcli( conCmd
                    , scanCmd
                    , knownCmd)
import Network.Types( SSID
                    , Log
                    , Command(..))
import Network.HWifi ( runWifiMonad
                     , elect
                     , alreadyUsed
                     , available)

-- | Returns the available network wifi list and records any logged message
availableWifisWithLogs :: Command -> IO ([SSID], [Log])
availableWifisWithLogs scanCmd = runWifiMonad $ available scanCmd

-- | Returns already used network wifi list and record any logged message.
alreadyUsedWifisWithLogs :: Command -> IO ([SSID], [Log])
alreadyUsedWifisWithLogs knownCmd = runWifiMonad $ alreadyUsed knownCmd

-- | Connect to the wifi
connectWifi :: Command -> SSID -> IO [SSID]
connectWifi cmd = run . connect cmd

-- | Log informational
output :: [Log]-> IO ()
output = mapM_ putStrLn

-- | Main orchestrator
-- Determine the highest known wifi signal and connect to it
main :: IO ()
main = do
  (allWifis, log)   <- availableWifisWithLogs scanCmd
  output log
  (knownWifis, log) <- alreadyUsedWifisWithLogs knownCmd
  output log
  let elected = elect knownWifis allWifis
  _ <- join $ connectWifi conCmd <$> elected
  elected >>= putStrLn . ("\n Elected Wifi: "++)

-- | Returns available network wifis. It discards any logged message.
availableWifis :: Command -> IO [SSID]
availableWifis scanCmd = fst <$> availableWifisWithLogs scanCmd

-- | Returns already used network wifis. It discards any logged message.
alreadyUsedWifis :: Command -> IO [SSID]
alreadyUsedWifis knownCmd = fst <$> alreadyUsedWifisWithLogs knownCmd

-- | Returns elected wifi (wifi already known, available, with highest signal).
electedWifi :: Command -> Command -> IO SSID
electedWifi scanCmd knownCmd = join $ elect <$> alreadyUsedWifis knownCmd <*> availableWifis scanCmd
