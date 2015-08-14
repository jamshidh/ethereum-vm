{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as SHA3
import Data.Time.Clock
import qualified Data.ByteString as B
import qualified Database.LevelDB as DB
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E
import HFlags
import System.Directory
import System.FilePath

import Blockchain.BlockChain
import Blockchain.Constants
import Blockchain.Data.Address
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.DB.DetailsDB
import Blockchain.Data.Transaction
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB
import Blockchain.DBM
import Blockchain.Format
import Blockchain.Options
import Blockchain.SHA
import Blockchain.Verifier
import Blockchain.VMContext
import qualified Network.Haskoin.Internals as H

coinbasePrvKey::H.PrvKey
Just coinbasePrvKey = H.makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

getNextBlock::Block->[Transaction]->IO Block
getNextBlock b transactions = do
  ts <- getCurrentTime
  let theCoinbase = prvKey2Address coinbasePrvKey
      bd = blockBlockData b
  return Block{
               blockBlockData=
               BlockData {
                 blockDataParentHash=blockHash b,
                 blockDataUnclesHash=hash$ B.pack [0xc0],
                 blockDataCoinbase=prvKey2Address coinbasePrvKey,
                 blockDataStateRoot = MP.SHAPtr "",
                 blockDataTransactionsRoot = MP.emptyTriePtr,
                 blockDataReceiptsRoot = MP.emptyTriePtr,
                 blockDataLogBloom = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],         
                 blockDataDifficulty = nextDifficulty (blockDataDifficulty bd) (blockDataTimestamp bd) ts,
                 blockDataNumber = blockDataNumber bd + 1,
                 blockDataGasLimit = blockDataGasLimit bd,
                 blockDataGasUsed = 0,
                 blockDataTimestamp = ts,  
                 blockDataExtraData = 0,
                 blockDataMixHash = SHA 0,
                 blockDataNonce = 5
               },
               blockReceiptTransactions=transactions,
               blockBlockUncles=[]
             }


wrapTransactions::ContextM ()
wrapTransactions = do
  transactions <- getUnprocessedTransactions
  pool <- getSQLDB

  when (not $ null transactions) $ do
                     bestBlock <-getBestBlock
                     nextBlock <- liftIO $ getNextBlock bestBlock transactions
                     blockId <- putBlockLite nextBlock 
                     runResourceT $
                                  flip SQL.runSqlPool pool $ 
                                       E.update $ \t -> do
                                         E.set t [ RawTransactionBlockNumber E.=. E.val (fromInteger $ blockDataNumber $ blockBlockData nextBlock),
                                                   RawTransactionBlockId E.=. E.val (blockId)]
                                         E.where_ (t E.^. RawTransactionBlockNumber E.==. E.val (-1))
                     return ()



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
                           (sqlDB' dbs)) $ 
          forever $ do
            blocks <- getUnprocessedBlocks
            forM_ blocks $ addBlock False

            when (flags_wrapTransactions) wrapTransactions

            when (length blocks < 100) $ liftIO $ threadDelay 5000000

  return ()

getUnprocessedBlocks::ContextM [Block]
getUnprocessedBlocks = do
  db <- getSQLDB
  blocks <-
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
      
  return $ map E.entityVal blocks

getUnprocessedTransactions::ContextM [Transaction]
getUnprocessedTransactions = do
  db <- getSQLDB
  transactions <-
    runResourceT $
    flip SQL.runSqlPool db $ 
    E.select $
    E.from $ \transaction -> do
      E.where_ (transaction E.^. RawTransactionBlockNumber E.==. E.val (-1))
      E.orderBy [E.asc (transaction E.^. RawTransactionNonce)]
      E.limit 1000
      return transaction
      
  return $ map (rawTX2TX . E.entityVal) transactions

  
