module Indexer where

import Data.ByteString ( ByteString )
import Data.ByteString.Lazy ( fromStrict, toStrict )

import Data.Binary.Get ( runGet, getWord8, getWord16be, getWord32be, getRemainingLazyByteString )
import Data.Binary.Put ( runPut, putWord8, putWord16be, putWord32be, putByteString )

import Data.Tuple ( swap )

import Data.Word
import Control.Monad.State
import Data.Map

type RowLength = Word16
type EntryMap  = Map Word16 Word32

data IndexingState = IndexingState {
    entries :: EntryMap,
    rowNum  :: Word32
}

data Row =
    DataRow Word16 ByteString |
    EndRow

data IndexedRow =
    IxDataRow Word32 Word16 ByteString |
    IxEndRow |
    IxGroupRow Word32 Word16

endFlag    = 0x01
groupFlag  = 0x02

newState :: IndexingState
newState = IndexingState {
    rowNum = 0,
    entries = empty
}

parseRow :: ByteString -> Row
parseRow bs = runGet cmp (fromStrict bs)
    where cmp = do
            flag <- getWord8
            case flag of
                endFlag -> return EndRow
                _       -> do
                    grp <- getWord16be
                    rest <- fmap toStrict $ getRemainingLazyByteString
                    return $ DataRow grp rest

parseIndexedRow :: ByteString -> IndexedRow
parseIndexedRow bs = runGet cmp ( fromStrict bs )
    where cmp = do
            flag <- getWord8
            case flag of
                endFlag -> return IxEndRow
                groupFlag -> do
                    link <- getWord32be
                    grp  <- getWord16be
                    return $ IxGroupRow link grp 
                _ -> do
                    link <- getWord32be
                    grp  <- getWord16be
                    rest <- fmap toStrict $ getRemainingLazyByteString
                    return $ IxDataRow link grp rest

addIndexToRow :: Row -> IndexingState -> ( IndexedRow, IndexingState )
addIndexToRow EndRow (IndexingState em rn ) = ( IxEndRow, IndexingState em (rn+1) )
addIndexToRow (DataRow grp raw) (IndexingState em rn ) = ( IxDataRow link grp raw, IndexingState nem (rn+1) )
    where
        link = findWithDefault 0 grp em
        nem = insert grp rn em

generateIndexGroupRows :: EntryMap -> [ IndexedRow ]
generateIndexGroupRows em = fmap constructor $ toList em
    where constructor = uncurry $ flip IxGroupRow

deconstructIndexedRow :: IndexedRow -> ByteString
deconstructIndexedRow ir = toStrict $ runPut cmp
    where cmp = case ir of
            IxEndRow -> putWord8 endFlag
            IxGroupRow link grp -> do
                putWord8 groupFlag
                putWord32be link
                putWord16be grp
            IxDataRow link grp raw -> do
                putWord8 0
                putWord32be link
                putWord16be grp
                putByteString raw
