import System.Random
import qualified Data.ByteString.Lazy as BL (fromChunks, writeFile )
import Options
import Data.Word

import Loader
import Indexer
import Generator
import Transformer

data MainOptions = MainOptions {
    width  :: Word16,
    len    :: Integer,
    output :: String
}

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "width" 10
            "Data row width (excluding meta and grp fields)"
        <*> simpleOption "length" 10
            "Data length (number of rows)"
        <*> simpleOption "out" "unindexed.test" 
            "The output file to store the data"

main :: IO ()
main = runCommand $ \opts args -> do
    gen <- newStdGen
    let chunks = fmap deconstructRow $ fst $ generateRows ( width opts ) ( len opts ) gen
    BL.writeFile ( output opts ) $ BL.fromChunks $ chunks
