module CsvTable (
  CsvTable,
  defaultCsvParserOpts,
  fromFile
) where

import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import qualified Data.Vector          as V

type Row a = V.Vector a
type Table a = V.Vector (Row a)
data CsvTable a = CsvTable { rows :: Table a } deriving(Show)

data CsvParserOpts = CsvParserOpts {  delimeter     :: Char
                                    , hasHeader     :: Bool
                                    , noFirstColumn :: Bool
                                    , noLastColumn  :: Bool }

defaultCsvParserOpts :: CsvParserOpts
defaultCsvParserOpts = CsvParserOpts { delimeter = ',', hasHeader = False, noFirstColumn = False, noLastColumn = False }


fromFile :: String -> CsvParserOpts -> IO (Either String (CsvTable Double))
fromFile path opts = do
   let decodeOpts = DecodeOptions { decDelimiter = fromIntegral (ord $ delimeter opts) }
   csvData <- BL.readFile path
   let result = if hasHeader opts
                  then decodeWith decodeOpts HasHeader csvData
                  else decodeWith decodeOpts NoHeader csvData
   case result of Left err -> return (Left err)
                  Right v ->  return (Right CsvTable { rows = (v :: Table Double) })
