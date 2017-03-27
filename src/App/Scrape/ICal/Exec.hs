{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: App.Scrape.ICal.Exec
-- Description: executable entrypoint.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Scrape.ICal.Exec
       ( main
       ) where

import App.Scrape.ICal.Convert (toVEvent, makeCalendar, makeUID)
import App.Scrape.ICal.Parse
  ( ParseResult(..), Event(..),
    scrapeEventChecker
  )
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Class (Default(def))
import Data.Monoid ((<>))
import Data.Text (pack)
import qualified Data.Text.Lazy as TL
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
import Text.ICalendar (VCalendar, printICalendar, VEvent(..))
import qualified Text.ICalendar as ICal

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
aggregateToCalendar rets = fmap makeCalendar $ (mapM toVEvent' . concat) =<< mapM filterResult rets where
  filterResult (url, ParseSuccess eve) = return [(url, eve)]
  filterResult (url, p) = logWarn ("URL = " ++ url ++ " : " ++ show p) >> return []
  toVEvent' (url, event) = fmap (setDescription (pack url) event) $ toVEvent (makeUID url event) event
  setDescription url event vevent =
    vevent { veDescription = Just $ ICal.Description (makeDesc url event) Nothing Nothing def
           }
  makeDesc scrape_url event = TL.fromStrict $ scrape_url <> (maybe "" (\url -> "\n" <> url) $ eventURI event)
