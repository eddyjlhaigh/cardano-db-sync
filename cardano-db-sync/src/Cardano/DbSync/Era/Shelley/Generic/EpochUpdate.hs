module Cardano.DbSync.Era.Shelley.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , allegraEpochUpdate
  , maryEpochUpdate
  , shelleyEpochUpdate
  ) where

import           Cardano.DbSync.Config.Types (DbSyncEnv (..))
import           Cardano.DbSync.Era.Shelley.Generic.ProtoParams
import           Cardano.DbSync.Era.Shelley.Generic.Rewards
import           Cardano.DbSync.Era.Shelley.Generic.StakeDist

import           Data.Maybe (fromMaybe)

import           Ouroboros.Consensus.Block (EpochNo)
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley

data NewEpoch = NewEpoch
  { epoch :: !EpochNo
  , epochUpdate :: !(Maybe EpochUpdate)
  }

data EpochUpdate = EpochUpdate
  { euProtoParams :: !ProtoParams
  , euRewards :: !(Maybe Rewards)
  , euStakeDistribution :: !StakeDist
  , euNonce :: !Shelley.Nonce
  }

allegraEpochUpdate :: DbSyncEnv -> LedgerState (ShelleyBlock StandardAllegra) -> Maybe Rewards -> Maybe Shelley.Nonce -> EpochUpdate
allegraEpochUpdate env sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = allegraProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = allegraStakeDist env sls
    , euNonce = fromMaybe Shelley.NeutralNonce mNonce
    }

maryEpochUpdate :: DbSyncEnv -> LedgerState (ShelleyBlock StandardMary) -> Maybe Rewards -> Maybe Shelley.Nonce -> EpochUpdate
maryEpochUpdate env sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = maryProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = maryStakeDist env sls
    , euNonce = fromMaybe Shelley.NeutralNonce mNonce
    }

shelleyEpochUpdate :: DbSyncEnv -> LedgerState (ShelleyBlock StandardShelley) -> Maybe Rewards -> Maybe Shelley.Nonce -> EpochUpdate
shelleyEpochUpdate env sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = shelleyProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = shelleyStakeDist env sls
    , euNonce = fromMaybe Shelley.NeutralNonce mNonce
    }
