import System.Random
import qualified Data.ByteString.Lazy as BL (fromChunks, writeFile )
import Options
import Data.Word
import System.IO

import Loader
import Indexer
import Generator
import Transformer

data MainOptions = MainOptions {
    width  :: Word16,
    input  :: String,
    stage  :: String,
    output :: String
}

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "width" 10
            "Data row width (excluding meta and grp fields)"
        <*> simpleOption "in" "unindexed.test"   
            "The input file (unindexed)"
        <*> simpleOption "stage" "stage.test" 
            "The temporary staging file"
        <*> simpleOption "out" "indexed.test" 
            "The output file (indexed)"

index :: Word16 -> String -> Handle -> IO ()
index width out hdl = do
    ixed <- fmap pipeline $ readRowsReversed ( width + 2 ) hdl
    BL.writeFile out $ BL.fromChunks $ ixed
    where pipeline = (fmap ( deconstructIndexedRow width ) ) . indexRows . ( fmap constructRow )

rev :: Word16 -> String -> Handle -> IO ()
rev width out hdl = do
    raw <- readRowsReversed ( width + 6 ) hdl
    BL.writeFile out $ BL.fromChunks $ raw

main :: IO()
main = runCommand $ \opts args -> do
    _ <- withFile (input opts) ReadMode $ index ( width opts ) (stage opts )
    withFile (stage opts) ReadMode $ rev (width opts) (output opts)
