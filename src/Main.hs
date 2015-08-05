{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource
import HFlags

import Blockchain.BlockChain
import Blockchain.Context
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.DB.DetailsDB
import Blockchain.DB.SQLDB
import Blockchain.DBM
import Blockchain.Format
import Blockchain.Options
import Blockchain.SHA

import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E
import Database.Esqueleto.Internal.Language 

{-
  runResourceT $
    flip SQL.runSqlPool db $ do
      [SQL.Entity _ qq] <- SQL.selectList [BlockDataRefParentHash SQL.==. h] [SQL.LimitTo 1]

      return $ Just qq
-}

main::IO ()
main = do
  _ <- $initHFlags "The Ethereum Haskell Peer"

  _ <-
    runResourceT $ do
      dbs <- openDBs "h"
      flip runStateT (Context
                           (stateDB' dbs)
                           (hashDB' dbs)
                           undefined
                           (codeDB' dbs)
                           (sqlDB' dbs)
                           []) $ do
          b1 <- getGenesisBlockHash
          liftIO $ putStrLn $ "genesis block hash = " ++ show b1
          insertBlockRecursivly b1
          return ()
  return ()

insertBlockRecursivly::SHA->ContextM ()
insertBlockRecursivly hash = do
  childrenHashes <- getChildrenHashes hash
  liftIO $ putStrLn $ "child block hash = " ++ show childrenHashes
  forM_ childrenHashes $ \childHash -> do
    Just block <- getBlock childHash
    liftIO $ putStrLn $ "children of genesis block: " ++ format block
    addBlock False block
    insertBlockRecursivly childHash


getChildrenHashes::(HasSQLDB m, MonadResource m, MonadBaseControl IO m)=>SHA->m [SHA]
getChildrenHashes h = do
  db <- getSQLDB
  hashVals <-
    runResourceT $
    flip SQL.runSqlPool db $ 
    E.select $
    E.from $ \bdRef -> do
      E.where_ (bdRef E.^. BlockDataRefParentHash E.==. E.val h )
      return $ bdRef E.^. BlockDataRefHash

  return [x|Value x <- hashVals]
