{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource
import qualified Database.LevelDB as DB
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E
import HFlags
import System.Directory
import System.FilePath

import Blockchain.BlockChain
import Blockchain.Constants
import Blockchain.VMContext
import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB
import Blockchain.DBM
import Blockchain.Options


main::IO ()
main = do
  _ <- $initHFlags "The Ethereum Haskell Peer"

  _ <-
    runResourceT $ do
      dbs <- openDBs "h"
      homeDir <- liftIO getHomeDirectory                     
      sdb <- DB.open (homeDir </> dbDir "h" ++ stateDBPath)
             DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
      let hdb = sdb
          cdb = sdb
      flip runStateT (Context
                           MP.MPDB{MP.ldb=sdb, MP.stateRoot=error "undefined stateroor"}
                           hdb
                           cdb
                           (sqlDB' dbs)
                           []) $ 
          forever $ do
            blocks <- getUnprocessedBlocks
            forM_ blocks $ addBlock False
            when (length blocks < 100) $ liftIO $ threadDelay 5000000

  return ()

getUnprocessedBlocks::ContextM [Block]
getUnprocessedBlocks = do
  db <- getSQLDB
  hashVals <-
    runResourceT $
    flip SQL.runSqlPool db $ 
    E.select $
    E.from $ \(bd `E.InnerJoin` block `E.LeftOuterJoin` processed) -> do
      E.on (E.just (block E.^. BlockId) E.==. processed E.?. ProcessedBlockId)
      E.on (bd E.^. BlockDataRefBlockId E.==. block E.^. BlockId)
      E.where_ (E.isNothing (processed E.?. ProcessedId))
      E.orderBy [E.asc (bd E.^. BlockDataRefNumber)]
      E.limit 1000
      return block
      
  return $ map E.entityVal hashVals


  
