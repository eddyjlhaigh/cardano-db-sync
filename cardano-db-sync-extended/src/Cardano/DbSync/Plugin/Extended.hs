module Cardano.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where

import           Cardano.Prelude

import           Control.Monad.Logger (LoggingT)

import           Cardano.Sync.Util (traverseMEither)
import           Cardano.Sync.Error (DbSyncNodeError)

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)
import           Cardano.Sync (DbSyncNodePlugin (..))

extendedDbSyncNodePlugin :: SqlBackend -> DbSyncNodePlugin
extendedDbSyncNodePlugin backend =
  let defPlugin = defDbSyncNodePlugin backend
  in  defPlugin
        { plugOnStartup =
            plugOnStartup defPlugin
              ++ [\tracer -> fmap Right $ DB.runDbAction backend (Just tracer) $ epochPluginOnStartup tracer]

        , plugInsertBlock = \tracer dbSyncEnv ledgerStateVar blockDetails -> runExceptT $ do
            ExceptT $ (plugInsertBlock defPlugin) tracer dbSyncEnv ledgerStateVar blockDetails

            let allBlocks :: ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
                allBlocks = traverseMEither (\blockDetail -> epochPluginInsertBlock tracer dbSyncEnv ledgerStateVar blockDetail) blockDetails

            ExceptT $ DB.runDbAction backend (Just tracer) $ allBlocks

        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock]
        }

