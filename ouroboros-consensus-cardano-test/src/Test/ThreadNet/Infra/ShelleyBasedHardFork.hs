{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test infrastructure to test hard-forking from one Shelley-based era to
-- another, e.g., Shelley to Allegra.
module Test.ThreadNet.Infra.ShelleyBasedHardFork (
    -- * Blocks
    ShelleyBasedHardForkEras
  , ShelleyBasedHardForkBlock
    -- * Transactions
  , pattern GenTxShelley1
  , pattern GenTxShelley2
    -- * Node
  , ShelleyBasedHardForkConstraints
  , protocolInfoShelleyAllegra
  ) where

import           Control.Monad.Except (runExcept)
import qualified Data.Map as Map
import           Data.SOP.Strict
import           Data.Void (Void)

import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.IOLike (IOLike)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Binary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
import qualified Ouroboros.Consensus.HardFork.History as History

import qualified Cardano.Ledger.Era as SL
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol

import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ShelleyPartialLedgerConfig (..), forecastAcrossShelley,
                     translateChainDepStateAcrossShelley)
import           Ouroboros.Consensus.Cardano.Node
                     (ProtocolParamsTransition (..), TriggerHardFork (..))

import           Test.ThreadNet.TxGen
import           Test.ThreadNet.TxGen.Shelley ()

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | Two eras, both Shelley-based.
type ShelleyBasedHardForkEras era1 era2 =
    '[ShelleyBlock era1, ShelleyBlock era2]

type ShelleyBasedHardForkBlock era1 era2 =
  HardForkBlock (ShelleyBasedHardForkEras era1 era2)

{-------------------------------------------------------------------------------
  Pattern synonyms, for encapsulation and legibility
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkGenTx era1 era2 =
  GenTx (ShelleyBasedHardForkBlock era1 era2)

pattern GenTxShelley1 ::
     GenTx (ShelleyBlock era1)
  -> ShelleyBasedHardForkGenTx era1 era2
pattern GenTxShelley1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley2 ::
     GenTx (ShelleyBlock era2)
  -> ShelleyBasedHardForkGenTx era1 era2
pattern GenTxShelley2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley1, GenTxShelley2 #-}

pattern ShelleyBasedHardForkNodeToNodeVersion1 ::
     BlockNodeToNodeVersion (ShelleyBasedHardForkBlock era1 era2)
pattern ShelleyBasedHardForkNodeToNodeVersion1 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

pattern ShelleyBasedHardForkNodeToClientVersion1 ::
     BlockNodeToClientVersion (ShelleyBasedHardForkBlock era1 era2)
pattern ShelleyBasedHardForkNodeToClientVersion1 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* Nil
      )

{-------------------------------------------------------------------------------
  Consensus instances
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkConstraints era1 era2 =
  ( ShelleyBasedEra era1
  , ShelleyBasedEra era2
  , EraCrypto era1 ~ EraCrypto era2
  , SL.PreviousEra era2 ~ era1

  , SL.TranslateEra       era2 SL.Tx
  , SL.TranslateEra       era2 SL.NewEpochState
  , SL.TranslationError   era2 SL.NewEpochState ~ Void
  , SL.TranslationContext era2 ~ ()
  )

instance ShelleyBasedHardForkConstraints era1 era2
      => SerialiseHFC (ShelleyBasedHardForkEras era1 era2)
   -- use defaults

instance ShelleyBasedHardForkConstraints era1 era2
      => CanHardFork (ShelleyBasedHardForkEras era1 era2) where
  hardForkEraTranslation = EraTranslation {
        translateLedgerState   = PCons translateLedgerState                PNil
      , translateChainDepState = PCons translateChainDepStateAcrossShelley PNil
      , translateLedgerView    = PCons translateLedgerView                 PNil
      }
    where
      translateLedgerState :: ()
        => InPairs.RequiringBoth
             WrapLedgerConfig
             (HFC.Translate LedgerState)
             (ShelleyBlock era1)
             (ShelleyBlock era2)
      translateLedgerState = InPairs.ignoringBoth $ HFC.Translate $ \_epochNo ->
          unComp . SL.translateEra' () . Comp

      translateLedgerView ::
          InPairs.RequiringBoth
            WrapLedgerConfig
            (HFC.TranslateForecast LedgerState WrapLedgerView)
            (ShelleyBlock era1)
            (ShelleyBlock era2)
      translateLedgerView =
          InPairs.RequireBoth $ \(WrapLedgerConfig cfg1) (WrapLedgerConfig cfg2) ->
            HFC.TranslateForecast $ forecastAcrossShelley cfg1 cfg2

  hardForkChainSel = Tails.mk2 SelectSameProtocol

  hardForkInjectTxs = InPairs.mk2 $ InPairs.ignoringBoth (InjectTx translateTx)
    where
      translateTx ::
           GenTx (ShelleyBlock era1)
        -> Maybe (GenTx (ShelleyBlock era2))
      translateTx =
          fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

instance ShelleyBasedHardForkConstraints era1 era2
      => SupportedNetworkProtocolVersion (ShelleyBasedHardForkBlock era1 era2) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToNodeVersion1)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToClientVersion1)
      ]

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyAllegra ::
     forall c m. (IOLike m, PraosCrypto c)
  => ProtocolParamsShelley c Identity
  -> ProtocolParamsAllegra c Identity
  -> ProtocolParamsTransition (ShelleyBlock (ShelleyEra c)) (ShelleyBlock (AllegraEra c))
  -> ProtocolInfo m (ShelleyBasedHardForkBlock (ShelleyEra c) (AllegraEra c))
protocolInfoShelleyAllegra protocolParamsShelley
                           protocolParamsAllegra
                           protocolParamsTransition =
    protocolInfoBinary
      -- Shelley
      protocolInfoShelley'
      eraParamsShelley
      tpraosParams
      toPartialLedgerConfigShelley
      -- Allegra
      protocolInfoAllegra
      eraParamsAllegra
      tpraosParams
      toPartialLedgerConfigAllegra
  where
    -- Shelley

    protocolInfoShelley' :: ProtocolInfo m (ShelleyBlock (ShelleyEra c))
    protocolInfoShelley' = protocolInfoShelley protocolParamsShelley

    eraParamsShelley :: History.EraParams
    eraParamsShelley = shelleyEraParams (shelleyGenesis protocolParamsShelley)

    ProtocolParamsTransition { transitionTrigger } = protocolParamsTransition

    toPartialLedgerConfigShelley ::
         LedgerConfig (ShelleyBlock (ShelleyEra c))
      -> PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
    toPartialLedgerConfigShelley cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = transitionTrigger
        }

    -- Allegra

    genesisAllegra :: SL.ShelleyGenesis (AllegraEra c)
    genesisAllegra =
        SL.translateEra' () $ shelleyGenesis protocolParamsShelley

    protocolInfoAllegra :: ProtocolInfo m (ShelleyBlock (AllegraEra c))
    protocolInfoAllegra =
        protocolInfoShelleyBased
          genesisAllegra
          (shelleyInitialNonce      protocolParamsShelley)
          (allegraProtVer           protocolParamsAllegra)
          (allegraLeaderCredentials protocolParamsAllegra)

    eraParamsAllegra :: History.EraParams
    eraParamsAllegra = shelleyEraParams genesisAllegra

    toPartialLedgerConfigAllegra ::
         LedgerConfig (ShelleyBlock (AllegraEra c))
      -> PartialLedgerConfig (ShelleyBlock (AllegraEra c))
    toPartialLedgerConfigAllegra cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = TriggerHardForkNever
        }

{-------------------------------------------------------------------------------
  TxGen instance
-------------------------------------------------------------------------------}

-- | Use a generic implementation for 'TxGen'
instance ( TxGen (ShelleyBlock era1)
         , TxGen (ShelleyBlock era2)
         , ShelleyBasedHardForkConstraints era1 era2
         ) => TxGen (ShelleyBasedHardForkBlock era1 era2) where
  type TxGenExtra (ShelleyBasedHardForkBlock era1 era2) =
    NP WrapTxGenExtra (ShelleyBasedHardForkEras era1 era2)
  testGenTxs = testGenTxsHfc
