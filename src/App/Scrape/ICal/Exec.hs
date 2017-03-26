-- |
-- Module: App.Scrape.ICal.Exec
-- Description: executable entrypoint.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Scrape.ICal.Exec
       ( main
       ) where

import App.Scrape.ICal.Convert (toVEvent, makeCalendar)
import App.Scrape.ICal.Parse
  ( ParseResult(..), Event(..),
    scrapeEventChecker
  )
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Class (Default(def))
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client
  ( Manager,
    httpLbs,
    parseUrlThrow,
    responseBody,
    newManager, defaultManagerSettings
  )
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Text.ICalendar (VCalendar, printICalendar)

main :: IO ()
main = scrapeURLs =<< getArgs


type URLString = String

scrapeURLs :: [URLString] -> IO ()
scrapeURLs urls = do
  man <- newManager defaultManagerSettings
  (BSL.putStr . printICalendar def) =<< (aggregateToCalendar . zip urls) =<< mapM (fetchAndScrape man) urls

fetchAndScrape :: Manager -> URLString -> IO (ParseResult Event)
fetchAndScrape man url = do
  req <- parseUrlThrow url
  res <- httpLbs req man
  return $ scrapeEventChecker url $ decodeUtf8 $ BSL.toStrict $ responseBody res

logWarn :: String -> IO ()
logWarn msg = hPutStrLn stderr ("[warn] " ++ msg)

aggregateToCalendar :: [(URLString, ParseResult Event)] -> IO VCalendar
aggregateToCalendar rets = fmap makeCalendar $ (mapM toVEvent . concat) =<< mapM filterResult rets where
  filterResult (_, ParseSuccess eve) = return [eve]
  filterResult (url, p) = logWarn ("URL = " ++ url ++ " : " ++ show p) >> return []
