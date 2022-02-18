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

module Week05.Homework2 where
import           Data.Aeson             (ToJSON, FromJSON)
import           GHC.Generics           (Generic)
import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx = traceIfFalse "UTxO not consumed" hasUTxO && traceIfFalse "Name is not empty" checkMintedAmount  -- FIX ME!

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    -- Makes sure Token has a UTxO. Takes the transaction input and returns reference. 
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    -- Mkes sure your token name is an empty bite string and only 1 is everminted.
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn, amt)] -> unTokenName tn == emptyByteString && amt == 1
        _              -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' -> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref

-- Creates Currency symbol from the minting policy
curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref = scriptCurrencySymbol $ policy oref -- IMPLEMENT ME!



type NFTSchema = Endpoint "mint" Address


mint :: Address -> Contract w NFTSchema Text ()
mint np = do -- IMPLEMENT ME!
    utxos <- utxosAt $ np
    case Map.keys utxos of
        -- If UTxO output is empty log no utxo found
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            -- Set the value of the currency symbol and token name
            let val     = Value.singleton (curSymbol oref) (TokenName emptyByteString) 1
                -- combines the minting policy and the unspet UTxOs. This is the policy for validation to be possible.
                lookups = Constraints.mintingPolicy (policy oref) <> Constraints.unspentOutputs utxos
                -- transaction must mint value and must spend the pub key output. This put the restrictions on the transactions
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            -- Submit the transaction
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            -- Wait for confirmation
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ mockWalletAddress w1
    callEndpoint @"mint" h2 $ mockWalletAddress w2
    void $ Emulator.waitNSlots 1
