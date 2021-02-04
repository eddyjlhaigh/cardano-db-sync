{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Cardano.DbSync.Rollback
  ( rollbackToSlot
  , unsafeRollback
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackToSlot :: Trace IO Text -> CardanoPoint -> IO (Either DbSyncNodeError ())
rollbackToSlot trce point =
    DB.runDbNoLogging $ runExceptT action
  where
    action :: MonadIO m => ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action = do
        liftIO $ logInfo trce msg
        xs <- lift $ slotsToDelete (pointSlot point)
        if null xs then
          liftIO $ logInfo trce "No Rollback is necessary"
        else do
          liftIO . logInfo trce $
              mconcat
                [ "Deleting slots numbered: ", renderSlotList xs
                ]
          mapM_ (lift . DB.deleteCascadeSlotNo) xs
          liftIO $ logInfo trce "Slots deleted"

    slotsToDelete Origin = DB.querySlotNos
    slotsToDelete (At sl) = DB.querySlotNosGreaterThan (unSlotNo sl)

    msg :: Text
    msg = case getPoint point of
            Origin -> "Rolling back to genesis"
            At blk -> mconcat
                  [ "Rolling back to "
                  , show $ blockPointSlot blk
                  , ", hash "
                  , renderByteArray $ toRawHash (Proxy @CardanoBlock) $ blockPointHash blk
                  ]

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
unsafeRollback trce slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow slotNo
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
