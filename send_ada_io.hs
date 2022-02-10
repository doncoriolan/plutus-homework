{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet
import System.IO

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace pp2 pp3 = do
   m1 <- activateContractWallet (knownWallet 1) payContract
   callEndpoint @"pay" m1 $ PayParams
       { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet pp2
       , ppLovelace = pp3
       }
   void $ waitUntilSlot 1


main :: IO ()
main = do
   putStrLn "Hello Wallet 1! Who would you like to send money to?"
   putStrLn "Enter a number from 2 to 10"
   walnum <- getLine
   putStrLn "How much ADA in lovelace would you like to send?"
   pp1 <- getLine
   let x = (read walnum :: Integer)
   let y = (read pp1 :: Integer)
   runEmulatorTraceIO $ payTrace x y


