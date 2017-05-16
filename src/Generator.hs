module Generator ( generateRows ) where

import Control.Monad.State
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy ( toStrict )
import System.Random
import Data.Word
import Data.Binary.Put

import Data

generateByteString :: RandomGen g => Word16 -> g -> ( ByteString, g )
generateByteString len rnd = ( toStrict $ runPut (mapM_ putWord8 numbers), std )
    where (numbers,std) = runState (mapM ( const $ state $ random ) $ take ( fromIntegral len ) [0..]) rnd
    
generateRow :: RandomGen g => Word16 -> g -> ( Row , g )
generateRow len rnd = runState cmp rnd
    where cmp = do
            grp <- state $ randomR ( 1 , 2 )
            bs  <- state $ generateByteString len
            return $ DataRow grp bs     

generateRows :: RandomGen g => Word16 -> Integer -> g -> ( [ Row ], g )
generateRows len numRows rnd = runState ( mapM (const $ state $ generateRow len ) $ enumFromTo 0 (numRows-1) ) rnd
