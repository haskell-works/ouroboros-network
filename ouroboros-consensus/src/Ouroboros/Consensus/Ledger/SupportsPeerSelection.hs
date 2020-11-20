{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Ledger.SupportsPeerSelection (
    LedgerSupportsPeerSelection (..)
  , PoolStake
  , getTipTime
    -- * Re-exports for convenience
  , DomainAddress (..)
  , PortNumber
  , RelayAddress (..)
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Time (UTCTime)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (DomainAddress (..), PoolStake, PortNumber,
                     RelayAddress (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WallClock
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import qualified Ouroboros.Consensus.HardFork.Abstract as HF
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract (LedgerState, UpdateLedger,
                     ledgerTipSlot)

class LedgerSupportsPeerSelection blk where
  -- | Return peers registered in the ledger ordered by descending 'PoolStake'.
  --
  -- For example, for Shelley, the relays that have been registered in the
  -- ledger for the respective stake pools will be returned.
  --
  -- Ledgers/blocks that don't support staking can return an empty list.
  getPeers :: LedgerState blk -> [(PoolStake, NonEmpty RelayAddress)]

-- | Return the time corresponding to the slot of the tip of the given ledger
-- state.
getTipTime ::
     forall blk.
     ( HF.HasHardForkHistory blk
     , ConfigSupportsNode blk
     , UpdateLedger blk
     , HasCallStack
     )
  => TopLevelConfig blk
  -> LedgerState blk
  -> UTCTime
getTipTime cfg st = case ledgerTipSlot st of
    Origin ->
      WallClock.getSystemStart systemStart
    NotOrigin slot ->
      case History.runQuery (History.slotToWallclock slot) summary of
        -- The slot at the tip of the ledger cannot be past the horizon
        Left _err          -> error "getTipTime: impossible"
        Right (relTime, _) -> WallClock.fromRelativeTime systemStart relTime
  where
    systemStart :: WallClock.SystemStart
    systemStart = getSystemStart (configBlock cfg)

    summary :: History.Summary (HF.HardForkIndices blk)
    summary = HF.hardForkSummary (configLedger cfg) st
