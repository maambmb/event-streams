module Data where

import Data.Word
import Data.Map
import Data.ByteString (ByteString)

type RowLength = Word16
type EntryMap  = Map Word8 Word32

data Row = DataRow Word8 ByteString

data IndexedRow =
    IxDataRow Word32 Word8 ByteString |
    IxGroupRow Word32 Word8

data IndexingState = IndexingState {
    entries :: EntryMap,
    rowNum  :: Word32
}

groupFlag :: Word8
groupFlag  = 0x01

newState :: IndexingState
newState = IndexingState {
    rowNum  = 0,
    entries = empty
}
