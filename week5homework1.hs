{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework1 where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Default               (Default (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import           Ledger.Constraints         as Constraints
import           Ledger.TimeSlot
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Prelude                    (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet
import           Plutus.V1.Ledger.Interval

{-# INLINABLE mkPolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PaymentPubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkPolicy :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy paykh dl () ctx = traceIfFalse "Owner has not signed the transaction" signedbyOwner && traceIfFalse "The deadline has passed" stillunderDeadline -- FIX ME!
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedbyOwner :: Bool
    -- make sure the transaction is signed it takes the TxInfo and pubkeyhas and returns a bool
    signedbyOwner = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash paykh

    stillunderDeadline :: Bool
    -- to makes sure the values are less than or equal to. 
    -- makes sure the deadline values are less than the posixs time range
    stillunderDeadline = to dl `contains` txInfoValidRange info

-- plutus core code.
policy :: PaymentPubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy paykh dl = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \paykh' dl' -> Scripts.wrapMintingPolicy $ mkPolicy paykh' dl' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode paykh
    `PlutusTx.applyCode`
    PlutusTx.liftCode dl -- IMPLEMENT ME!

-- creates currency symbol by applying policy to it fist.
curSymbol :: PaymentPubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol paykh dl = scriptCurrencySymbol $ policy paykh dl -- IMPLEMENT ME!

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpDeadline  :: !POSIXTime
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    now <- Contract.currentTime
    let deadline = mpDeadline mp
    if now > deadline
        then Contract.logError @String "deadline passed"
        else do
            -- apply value to value to create NFT
            let val     = Value.singleton (curSymbol pkh deadline) (mpTokenName mp) (mpAmount mp)
                -- the policy for validation to be posibble 
                lookups = Constraints.mintingPolicy $ policy pkh deadline
                -- the transaction must mint the value. Also wait a few seconds after the transaction
                tx      = Constraints.mustMintValue val <> Constraints.mustValidateIn (to $ now + 60000)
            -- submit the transaction
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            -- wait for confirmation 
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn       = "ABC"
        deadline = slotToBeginPOSIXTime def 100
    h <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 110
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 
