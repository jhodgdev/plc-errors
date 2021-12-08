{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}

module PlcErrors (
  treasury,
) where

import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes (..),
  mkTypedValidatorParam,
  wrapValidator,
 )
import Plutus.V1.Ledger.Contexts (
  ScriptContext,
  TxInfo,
  scriptContextTxInfo,
  valueLockedBy,
  valuePaidTo,
 )
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
 )
import Plutus.V1.Ledger.Value (
  AssetClass,
  Value,
  assetClass,
  assetClassValue,
 )
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude (Show)

data Epoch = Epoch
  { epochNumber :: Integer
  }

instance Eq Epoch where
  {-# INLINEABLE (==) #-}
  e == e' = epochNumber e == epochNumber e'

data TreasuryState = TreasuryState
  { trsExecutive :: Credential
  , lastRewardEpoch :: Epoch
  }

instance Eq TreasuryState where
  {-# INLINEABLE (==) #-}
  ts == ts' =
    trsExecutive ts == trsExecutive ts'
      && lastRewardEpoch ts == lastRewardEpoch ts'

PlutusTx.unstableMakeIsData ''TreasuryState

data TreasuryRedeemer = IssueRewards

data TreasuryScript
instance ValidatorTypes TreasuryScript where
  type DatumType TreasuryScript = TreasuryState
  type RedeemerType TreasuryScript = TreasuryRedeemer

data TreasuryParams = TreasuryParams
  { trAssetClass :: !AssetClass
  , trpExecutive :: !Credential
  , firstEpoch :: !Epoch
  }

data Treasury = Treasury
  { treasuryScript :: TypedValidator TreasuryScript
  }
  deriving (Show)

{-# INLINEABLE valuePaidToCredential #-}
valuePaidToCredential :: TxInfo -> Credential -> Value
valuePaidToCredential i c = case c of
  PubKeyCredential pkh -> valuePaidTo i pkh
  ScriptCredential vh -> valueLockedBy i vh

{-# INLINEABLE validator #-}
validator ::
  TreasuryParams -> TreasuryState -> TreasuryRedeemer -> ScriptContext -> Bool
validator p s IssueRewards ctx =
  traceIfFalse
    "Unexpected value rewarded."
    (valueRewarded == expectedReward)
  where
    txInfo = scriptContextTxInfo ctx
    expectedReward = assetClassValue (trAssetClass p) 100
    valueRewarded = valuePaidToCredential txInfo $ trsExecutive s

mkScript :: TreasuryParams -> TypedValidator TreasuryScript
mkScript =
  mkTypedValidatorParam @TreasuryScript
    $$(PlutusTx.compile [||validator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @TreasuryState @TreasuryRedeemer

mkTreasury :: TreasuryParams -> Treasury
mkTreasury p =
  Treasury
    { treasuryScript = mkScript p
    }

ada :: AssetClass
ada = assetClass "" ""

tp :: TreasuryParams
tp =
  TreasuryParams
    { trAssetClass = ada
    , trpExecutive = ScriptCredential ""
    , firstEpoch = Epoch 0
    }

treasury :: Treasury
treasury = mkTreasury tp

PlutusTx.makeLift ''Epoch
PlutusTx.makeLift ''TreasuryParams

PlutusTx.unstableMakeIsData ''Epoch
PlutusTx.unstableMakeIsData ''TreasuryRedeemer
