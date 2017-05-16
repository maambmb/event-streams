module Indexer ( indexRows ) where

import Control.Monad
import Control.Monad.State
import Data.Map

import Data

addIndexToRow :: Row -> IndexingState -> ( IndexedRow, IndexingState )
addIndexToRow (DataRow grp raw) (IndexingState em rn ) = ( IxDataRow ( rn - link ) grp raw, IndexingState nem (rn+1) )
    where
        link = findWithDefault rn grp em
        nem = insert grp rn em

generateIndexGroupRows :: IndexingState -> [ IndexedRow ]
generateIndexGroupRows is = fmap constructor src
    where 
        rn = rowNum is
        constructor ( (k,v), i ) = IxGroupRow ( rn - v + i ) k
        src = zip ( toList $ entries is ) [0..]


indexRows :: [ Row ] -> [ IndexedRow ]
indexRows rows = irows ++ grpRows
    where 
        cmp        = mapM ( state . addIndexToRow ) rows
        (irows,st) = runState cmp $ newState
        grpRows    = generateIndexGroupRows st
