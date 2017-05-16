module Transformer where

import Data.ByteString ( ByteString )
import Data.ByteString.Lazy ( fromStrict, toStrict )

import Data.Binary.Get 
import Data.Binary.Put 
import Data.Word
import Control.Monad.State
import Data.Map

import Data

constructRow :: ByteString -> Row
constructRow bs = runGet cmp (fromStrict bs)
    where cmp = do
            _    <- getWord8
            grp  <- getWord8
            rest <- fmap toStrict $ getRemainingLazyByteString
            return $ DataRow grp rest

deconstructRow :: Row -> ByteString
deconstructRow ( DataRow grp raw ) = toStrict $ runPut cmp
    where cmp = do
            putWord8 0
            putWord8 grp
            putByteString raw

constructIndexedRow :: ByteString -> IndexedRow
constructIndexedRow bs = runGet cmp ( fromStrict bs )
    where cmp = do
            flag <- getWord8
            case flag of
                x | x == groupFlag -> do
                    link <- getWord32be
                    grp  <- getWord8
                    return $ IxGroupRow link grp 
                _ -> do
                    link <- getWord32be
                    grp  <- getWord8
                    rest <- fmap toStrict $ getRemainingLazyByteString
                    return $ IxDataRow link grp rest

deconstructIndexedRow :: Word16 -> IndexedRow -> ByteString
deconstructIndexedRow width ir = toStrict $ runPut cmp
    where cmp = case ir of
            IxGroupRow link grp -> do
                putWord8 groupFlag
                putWord32be link
                putWord8 grp
                mapM_ ( const $ putWord8 0 ) ( take (fromIntegral width) [0..] )
            IxDataRow link grp raw -> do
                putWord8 0
                putWord32be link
                putWord8 grp
                putByteString raw
