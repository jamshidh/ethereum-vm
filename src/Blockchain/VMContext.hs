{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.VMContext (
  Context(..),
  ContextM,
--  getDebugMsg,
--  clearDebugMsg,
  incrementNonce,
  getNewAddress
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.State
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import qualified Blockchain.Database.MerklePatricia as MPDB
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.SQLDB
import Blockchain.DB.StateDB
import Blockchain.DB.StorageDB
import Blockchain.Options

--import Debug.Trace

data Context =
  Context {
    contextStateDB::MPDB.MPDB,
    contextHashDB::HashDB,
    contextCodeDB::CodeDB,
    contextSQLDB::SQLDB
    }

type ContextM = StateT Context (ResourceT IO)

instance HasStateDB ContextM where
  getStateDB = do
    cxt <- get
    return $ contextStateDB cxt
  setStateDBStateRoot sr = do
    cxt <- get
    put cxt{contextStateDB=(contextStateDB cxt){MPDB.stateRoot=sr}}

instance HasStorageDB ContextM where
  getStorageDB = do
    cxt <- get
    return $ MPDB.ldb $ contextStateDB cxt --storage and states use the same database!

instance HasHashDB ContextM where
  getHashDB = fmap contextHashDB get

instance HasCodeDB ContextM where
  getCodeDB = fmap contextCodeDB get

instance HasSQLDB ContextM where
  getSQLDB = fmap contextSQLDB get

{-
getDebugMsg::ContextM String
getDebugMsg = do
  cxt <- get
  return $ concat $ reverse $ vmTrace cxt

clearDebugMsg::ContextM ()
clearDebugMsg = do
  cxt <- get
  put cxt{vmTrace=[]}
-}

incrementNonce::(HasStateDB m, HasHashDB m)=>
                Address->m ()
incrementNonce address = do
  addressState <- getAddressState address
  putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }

getNewAddress::(HasStateDB m, HasHashDB m)=>
               Address->m Address
getNewAddress address = do
  addressState <- getAddressState address
  when flags_debug $ liftIO $ putStrLn $ "Creating new account: owner=" ++ show (pretty address) ++ ", nonce=" ++ show (addressStateNonce addressState)
  let newAddress = getNewAddress_unsafe address (addressStateNonce addressState)
  incrementNonce address
  return newAddress











