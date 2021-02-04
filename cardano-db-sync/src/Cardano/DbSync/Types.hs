{-# LANGUAGE DataKinds #-}
module Cardano.DbSync.Types
  ( BlockDetails (..)
  , CardanoBlock
  , CardanoPoint
  , CardanoProtocol
  , EpochSlot (..)
  , SlotDetails (..)
  , SyncState (..)
  , LedgerStateDir (..)
  , DbSyncProtocol (..)
  ) where

import           Cardano.DbSync.Config.Types

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Ouroboros.Network.Block (Point)

type CardanoPoint = Point CardanoBlock

data BlockDetails = BlockDetails
  { bdBlock :: !CardanoBlock
  , bdSlot :: !SlotDetails
  }

newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  } deriving (Eq, Show)

data SlotDetails = SlotDetails
  { sdSlotTime :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  } deriving (Eq, Show)

data SyncState
  = SyncLagging         -- Local tip is lagging the global chain tip.
  | SyncFollowing       -- Local tip is following global chain tip.
  deriving (Eq, Show)
