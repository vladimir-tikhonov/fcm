module Main where

import           Data.Matrix         as M
import           Data.Strings
import           Fcm.Csv
import           Fcm.Fcm
import           Fcm.Types
import           Options.Applicative
import           System.IO

data Opts = Opts
  { input      :: String
  , output     :: String
  , clusters   :: Int
  , metric     :: DistMethod
  , precision  :: Double
  , initMethod :: InitMethod
  } deriving(Show)

spec :: Parser Opts
spec = Opts
     <$> argument str
         (  metavar "FILE"
         <> help "Файл с данными для кластеризации." )
     <*> option str
         (  long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "Файл, куда будут записаны результаты."
         <> value "" )
     <*> option auto
         (  long "clusters"
         <> short 'c'
         <> metavar "COUNT"
         <> help "Количество кластеров, по умолчанию 5."
         <> value 5 )
     <*> option auto
         (  long "metric"
         <> short 'm'
         <> metavar "NAME"
         <> help "Используемая метрика (Euclid или Hamming), по умолчанию Euclid."
         <> value Euclid )
     <*> option auto
         (  long "precision"
         <> short 'p'
         <> metavar "VALUE"
         <> help "Точность вычислений, по умолчанию 0.0001."
         <> value 0.0001 )
     <*> option auto
         (  long "initializer"
         <> short 'i'
         <> metavar "NAME"
         <> help "Метод инициализации начальных значений (BelongingDegree или Centers), по умолчанию BelongingDegree."
         <> value BelongingDegree )

process :: Opts -> IO ()
process opts = do
  handle <- openFile (input opts) ReadMode
  hSetEncoding handle utf8_bom
  contents <- hGetContents handle
  csv <- builsFromString contents defaultCsvParserOpts
  _ <- case csv of
    Right table -> do
      let x = toDoublesMatrix table
          fcmOpts = buildFcmOpts opts
      result <- fcm fcmOpts x
      logResult opts result
    Left err -> putStrLn err
  hClose handle
  return ()

logResult :: Opts -> BelongingMatrix -> IO()
logResult opts u
  | strNull outputPath = putStrLn resultStr
  | otherwise = writeFile outputPath resultStr
  where resultStr = prettyMatrix u
        outputPath = output opts

buildFcmOpts :: Opts -> FcmOpts
buildFcmOpts opts = FcmOpts { c = clusters opts
                  , e = precision opts
                  , distMethod = metric opts
                  , Fcm.Fcm.initMethod = Main.initMethod opts }

main :: IO ()
main = execParser opts >>= process
 where
   opts = info (helper <*> spec)
     ( fullDesc
    <> progDesc "Кластеризует переданные данные методом FCM"
    <> header "fcm - утилита для кластеризации данных" )
