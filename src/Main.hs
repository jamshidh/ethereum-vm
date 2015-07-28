{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource
import HFlags

import Blockchain.BlockChain
import Blockchain.Context
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.GenesisBlock
import Blockchain.DB.DetailsDB
import Blockchain.DB.SQLDB
import Blockchain.DBM
import Blockchain.Format
import Blockchain.Options
import Blockchain.SHA

import qualified Database.Persist.Postgresql as SQL
import qualified Database.Persist.Sql as SQL
import qualified Database.Esqueleto as E

getBlockChildren::(HasSQLDB m, MonadResource m, MonadBaseControl IO m)=>SHA->m (Maybe Block)
getBlockChildren h = do
  db <- getSQLDB
  entBlkL <- runResourceT $
    flip SQL.runSqlPool db $ do
      E.select $ E.from $ \(bdRef, block) -> do
        E.where_ (bdRef E.^. BlockDataRefParentHash E.==. E.val h )
        return block          


  case entBlkL of
    [] -> return Nothing
    lst -> return $ Just . E.entityVal . head $ lst






main = do
  args <- $initHFlags "The Ethereum Haskell Peer"

  runResourceT $ do
      dbs <- openDBs "h"
      _ <- flip runStateT (Context
                           (stateDB' dbs)
                           (hashDB' dbs)
                           (blockDB' dbs)
                           (codeDB' dbs)
                           (sqlDB' dbs)
                           (detailsDB' dbs)
                           []) $ do
        b1 <- getGenesisBlockHash flags_altGenBlock
        liftIO $ putStrLn $ "genesis block hash = " ++ show b1
        Just block <- getBlockChildren b1
        liftIO $ putStrLn $ "children of genesis block: " ++ format block
        addBlock False block

--select * from block_data_ref where parent_hash like 'fd4af92a79c7fc2fd8bf0d342f2e832e1d4f485c85b9152d2039e03bc604fdca'

      return ()
