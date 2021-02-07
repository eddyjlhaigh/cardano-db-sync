module Cardano.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where

import           Cardano.Prelude

import           Control.Monad.Logger (LoggingT)

import           Cardano.Sync.Error (DbSyncNodeError)
import           Cardano.Sync.Util (traverseMEither)

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

            let insertBlockPlugin = plugInsertBlock defPlugin
            ExceptT $ insertBlockPlugin tracer dbSyncEnv ledgerStateVar blockDetails

            let allBlocks :: ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
                allBlocks = traverseMEither (epochPluginInsertBlock tracer dbSyncEnv ledgerStateVar) blockDetails

            ExceptT $ DB.runDbAction backend (Just tracer) allBlocks

        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock]
        }

