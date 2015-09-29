module CsvTable (
  defaultCsvParserOpts,
  fromFile,
  toDoublesList
) where

import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import qualified Data.Vector          as V

type Row a = V.Vector a
type Table a = V.Vector (Row a)

data CsvParserOpts = CsvParserOpts {  delimeter     :: Char
                                    , hasHeader     :: Bool
                                    , noFirstColumn :: Bool
                                    , noLastColumn  :: Bool }

defaultCsvParserOpts :: CsvParserOpts
defaultCsvParserOpts = CsvParserOpts { delimeter = ',', hasHeader = False, noFirstColumn = False, noLastColumn = True }

fromFile :: String -> CsvParserOpts -> IO (Either String (Table String))
fromFile path opts = do
   let decodeOpts = DecodeOptions { decDelimiter = fromIntegral (ord $ delimeter opts) }
   csvData <- BL.readFile path
   let result = if hasHeader opts
                  then decodeWith decodeOpts HasHeader csvData
                  else decodeWith decodeOpts NoHeader csvData
   case result of Left err -> return (Left err)
                  Right v ->  do
                    let t = applyFirstColumnOpts opts $ applyLastColumnOpts opts v
                    return (Right (t :: Table String) )

toDoublesList :: Table String -> [[Double]]
toDoublesList table =
    V.toList $ V.map V.toList doublesV
    where doublesV = V.map (V.map $ \x -> read x :: Double) $ table

applyFirstColumnOpts :: CsvParserOpts -> Table a -> Table a
applyFirstColumnOpts (CsvParserOpts _ _ False _) table = table
applyFirstColumnOpts (CsvParserOpts _ _ True _)  table =
    V.map V.tail table

applyLastColumnOpts :: CsvParserOpts -> Table a -> Table a
applyLastColumnOpts (CsvParserOpts _ _ _ False) table = table
applyLastColumnOpts (CsvParserOpts _ _ _ True)  table =
    V.map V.init table
