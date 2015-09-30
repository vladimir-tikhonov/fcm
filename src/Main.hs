module Main where

import           Data.Matrix         as M
import           Fcm.Csv
import           Fcm.Fcm
import           Options.Applicative

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
     <*> option auto
         (  long "metric"
         <> short 'm'
         <> metavar "NAME"
         <> help "Используемая метрика (Euclid или Hamming)"
         <> value Euclid )
     <*> option auto
         (  long "precision"
         <> short 'p'
         <> metavar "VALUE"
         <> help "Точность вычислений"
         <> value 0.01 )
     <*> option auto
         (  long "initializer"
         <> short 'i'
         <> metavar "NAME"
         <> help "Метод инициализации начальных значений (BelongingDegree или Centers)"
         <> value BelongingDegree )

showInput :: Opts -> IO ()
showInput opts = do
  csv <- fromFile (input opts) defaultCsvParserOpts
  _ <- case csv of
    Right table -> do
      let m = toDoublesMatrix table
          fcmOpts = FcmOpts { c = clusters opts
                            , e = precision opts
                            , distMethod = metric opts
                            , Fcm.Fcm.initMethod = Main.initMethod opts }
      result <- fcm fcmOpts m
      putStrLn $ prettyMatrix result
      return ()
    Left err -> putStrLn err
  return ()

main :: IO ()
main = execParser opts >>= showInput
 where
   opts = info (helper <*> spec)
     ( fullDesc
    <> progDesc "Кластеризует переданные данные методом FCM"
    <> header "fcm - утилита для кластеризации данных" )
