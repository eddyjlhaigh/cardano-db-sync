{-# LANGUAGE BangPatterns #-}

module Cardano.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)
import           Cardano.Sync (DbSyncNodePlugin (..))

extendedDbSyncNodePlugin :: SqlBackend -> DbSyncNodePlugin
extendedDbSyncNodePlugin backend =
  let !defPlugin = defDbSyncNodePlugin backend
  in  defPlugin
        { plugOnStartup =
            plugOnStartup defPlugin
              ++ [\tracer -> fmap Right $ DB.runDbAction backend (Just tracer) $ epochPluginOnStartup tracer]

        , plugInsertBlock =
            plugInsertBlock defPlugin
              ++ [\tracer env ledgerStateVar blockDetails -> DB.runDbAction backend (Just tracer) $ epochPluginInsertBlock tracer env ledgerStateVar blockDetails]

        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock]
        }

