module Network.Types where

import Control.Monad.Writer (WriterT)

type WifiMonad w a = WriterT w IO a
type SSID  = String
type Signal= String
type Wifi  = (SSID, Signal)
type Log   = String


-- | A CLI command to connect or scan wifi
type CLICommand = String

-- | A command is either to scan wifi or to connect to one.
data Command = Scan { scan :: CLICommand }
             | Connect { connect :: SSID -> CLICommand }

instance Show Command where
  show (Scan _) = "Scanning for finding some Wifi"
  show (Connect _) = "Connecting to an elected Wifi..."
