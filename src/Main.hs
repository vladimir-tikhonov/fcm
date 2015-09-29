module Main where

import           CsvTable
import           Options.Applicative

data Opts = Opts
  { input     :: String
  , output    :: String
  , clusters  :: Int
  , metric    :: String
  , precision :: Double
  } deriving(Show)

spec :: Parser Opts
spec = Opts
     <$> argument str
         (  metavar "FILE"
         <> help "Файл с данными для кластеризации" )
     <*> option str
         (  long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "Файл, куда будут записаны результаты"
         <> value "/dev/stdout" )
     <*> option auto
         (  long "clusters"
         <> short 'c'
         <> metavar "COUNT"
         <> help "Количество кластеров"
         <> value 5 )
     <*> option str
         (  long "metric"
         <> short 'm'
         <> metavar "NAME"
         <> help "Используемая метрика"
         <> value "euler" )
     <*> option auto
         (  long "precision"
         <> short 'p'
         <> metavar "VALUE"
         <> help "Точность вычислений"
         <> value 0.01 )

showInput :: Opts -> IO ()
showInput opts = do
  csv <- fromFile (input opts) defaultCsvParserOpts
  _ <- case csv of
    Right c -> putStrLn $ show $ toDouble c
    Left err -> putStrLn err
  return ()

main :: IO ()
main = execParser opts >>= showInput
 where
   opts = info (helper <*> spec)
     ( fullDesc
    <> progDesc "Кластеризует переданные данные методом FCM"
    <> header "fcm - утилита для кластеризации данных" )
