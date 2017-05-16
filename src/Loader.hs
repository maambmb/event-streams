module Loader (readRows,readRowsReversed) where 

import Data.ByteString ( ByteString, hGet )

import Data.Word
import System.IO

countRows :: Integer -> Integer -> Integer
countRows rowLength fs =
    case mod fs rowLength of
        0 -> res
        _ -> res + 1
    where res = div fs rowLength

readRow :: Word16 -> Handle -> Integer -> IO ByteString
readRow rowLength hdl n = do
    _  <- hSeek hdl AbsoluteSeek $ n * (fromIntegral rowLength)
    hGet hdl $ fromIntegral rowLength

readRows :: Word16 -> Handle -> IO [ ByteString ]
readRows rowLength hdl = do
        fileSize <- hFileSize hdl
        let numRows  = countRows (fromIntegral rowLength) fileSize
        let rowRange = enumFromTo 0 (numRows-1)
        mapM ( readRow rowLength hdl ) rowRange

readRowsReversed :: Word16 -> Handle -> IO [ ByteString ]
readRowsReversed rowLength hdl = do
        fileSize <- hFileSize hdl
        let numRows  = countRows (fromIntegral rowLength) fileSize
        let rowRange = enumFromThenTo (numRows-1) (numRows-2) 0
        mapM ( readRow rowLength hdl ) rowRange
