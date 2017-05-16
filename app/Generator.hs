
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as BL ( fromChunks, writeFile )

import Control.Monad.State
import System.IO
import Reverser
import Indexer

generate :: [ ByteString ] -> [ IndexedRow ]
generate bss = irows ++ grpRows
    where 
        rows       = fmap parseRow bss
        cmp        = mapM (\r -> state $ addIndexToRow r ) rows
        (irows,st) = runState cmp $ newState
        grpRows    = generateIndexGroupRows $ entries st

main :: IO()
main = withFile "test/input" ReadMode $ \hdl -> do
    raw <- Reverser.readRowsReversed 1 hdl
    let bs = fmap deconstructIndexedRow $ generate raw
    BL.writeFile "test/reversed" $ BL.fromChunks bs
