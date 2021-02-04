{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Environment
  ( DbSyncEnv (..)
  , LedgerEnv (..)
  , mkDbSyncEnvFromConfig
  , getLatestPoints
  ) where

import Cardano.Prelude (Proxy (..), catMaybes, find)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Shelley
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util (textShow)

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Crypto.ProtocolMagic

import           Data.ByteString (ByteString)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Block.Abstract (HeaderHash, fromRawHash)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley

data DbSyncEnv = DbSyncEnv
  { envProtocol :: !DbSyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envLedger :: LedgerEnv
  }

mkDbSyncEnv :: ProtocolInfo IO CardanoBlock -> Shelley.Network -> NetworkMagic -> SystemStart -> LedgerStateDir -> IO DbSyncEnv
mkDbSyncEnv protocolInfo network networkMagic systemStart dir = do
    latestSlot <- SlotNo <$> DB.runDbNoLogging DB.queryLatestSlotNo
    ledgerEnv <- mkLedgerEnv protocolInfo dir network latestSlot True
    return $ DbSyncEnv
      { envProtocol = DbSyncProtocolCardano
      , envNetworkMagic = networkMagic
      , envSystemStart = systemStart
      , envLedger = ledgerEnv
      }

mkDbSyncEnvFromConfig :: LedgerStateDir -> GenesisConfig -> IO (Either DbSyncNodeError DbSyncEnv)
mkDbSyncEnvFromConfig dir genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg
        | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic (scConfig sCfg) ->
            return $ Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                , " /= ", textShow (Shelley.sgNetworkMagic $ scConfig sCfg)
                ]
        | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart (scConfig sCfg) ->
            return $ Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                , " /= ", textShow (Shelley.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise -> Right <$> mkDbSyncEnv
                         (mkProtocolInfoCardano genCfg)
                         (Shelley.sgNetworkId (scConfig sCfg))
                         (NetworkMagic (unProtocolMagicId $ Byron.configProtocolMagicId bCfg))
                         (SystemStart (Byron.gdStartTime $ Byron.configGenesisData bCfg))
                         dir

getLatestPoints :: DbSyncEnv -> IO [CardanoPoint]
getLatestPoints env = do
    files <- listLedgerStateFilesOrdered $ leDir $ envLedger env
    catMaybes <$> mapM validLedgerFileToPoint files
  where
    validLedgerFileToPoint :: LedgerStateFile -> IO (Maybe CardanoPoint)
    validLedgerFileToPoint lsf = do
        hashes <- DB.runDbNoLogging $ DB.querySlotHash (lsfSlotNo lsf)
        let valid  = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
        case valid of
          Just (slot, hash) | slot == lsfSlotNo lsf -> return $ convert (slot, hash)
          _ -> return Nothing

    convert :: (SlotNo, ByteString) -> Maybe CardanoPoint
    convert (slot, hashBlob) =
      Point . Point.block slot <$> convertHashBlob hashBlob

    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)
