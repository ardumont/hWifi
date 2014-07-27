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
import Control.Monad(join)
import Data.Functor((<$>))
import Control.Applicative((<*>))
import Network.Utils(run)
import Network.Nmcli(conCmd, scanCmd, knownCmd)
import Network.Types(SSID, Log, Command(..))
import Network.HWifi (runWifiMonad,
                      safeElect,
                      alreadyUsed,
                      available)

-- | Returns the list of available network wifis and record any logged message
availableWifisWithLogs :: IO ([SSID], [Log])
availableWifisWithLogs =  runWifiMonad $ available scanCmd

-- | Returns available network wifis. It discards any logged message.
availableWifis :: IO [SSID]
availableWifis = fst <$> availableWifisWithLogs

-- | Returns the list of already used network wifis and record any logged message
alreadyUsedWifisWithLogs :: IO ([SSID], [Log])
alreadyUsedWifisWithLogs = runWifiMonad $ alreadyUsed knownCmd

-- | Returns already used network wifis. It discards any logged message.
alreadyUsedWifis :: IO [SSID]
alreadyUsedWifis = fst <$> alreadyUsedWifisWithLogs

-- | Returns the elected wifi : already used and available with high signal.
electedWifi :: IO SSID
electedWifi = join $ safeElect <$> alreadyUsedWifis <*> availableWifis

logAll:: [Log]-> IO ()
logAll = mapM_ putStrLn

main :: IO ()
main = do
  (allWifis, msg1)   <- availableWifisWithLogs
  logAll msg1
  (knownWifis, msg2) <- alreadyUsedWifisWithLogs
  logAll msg2
  let elected = safeElect knownWifis allWifis
  _ <- join $ run . connect conCmd <$> elected
  elected >>= putStrLn . ("\n Elected Wifi: "++)
