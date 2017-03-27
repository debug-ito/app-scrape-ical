-- |
-- Module: App.Scrape.ICal.Convert
-- Description: Covert events into ICal data structure.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Scrape.ICal.Convert
       ( toVEvent,
         makeUID,
         makeCalendar,
         addVEvent
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Data.Default.Class (Default(def))
import qualified Data.Map.Lazy as M
import Data.Monoid (mempty)
import Data.Text (Text, unpack, pack)
import Data.Text.Lazy (fromStrict)
import Data.Time (UTCTime, getCurrentTime, showGregorian)
import App.Scrape.ICal.Parse (Event(..))
import Network.BSD (getHostName)
import Network.URI (parseURI)
import Text.ICalendar (VEvent(..), Summary(..), Location(..), VCalendar(..))
import qualified Text.ICalendar as ICal

makeCalendar :: [VEvent] -> VCalendar
makeCalendar = foldr addVEvent def

addVEvent :: VEvent -> VCalendar -> VCalendar
addVEvent eve cal = cal { vcEvents = M.insert key eve $ vcEvents cal } where
  key = (ICal.uidValue $ veUID eve, fmap recurToEither $ veRecurId eve)
  recurToEither (ICal.RecurrenceIdDate d _ _) = Left d
  recurToEither (ICal.RecurrenceIdDateTime d _ _) = Right d

makeUID :: String -> Event -> IO Text
makeUID source_url event = do
  host <- getHostName
  return $ pack (source_url ++ "#" ++ start ++ "-" ++ end ++ "@" ++ host)
  where
    start = showGregorian $ fst $ eventWhen event
    end = showGregorian $ snd $ eventWhen event

-- | Create 'VEvent' from 'Event'. The DTSTAMP of 'VEvent' is set to
-- the current system time.
toVEvent :: Text -> Event -> IO VEvent
toVEvent uid event = makeVEvent uid <$> getCurrentTime <*> pure event

makeVEvent :: Text -> UTCTime -> Event -> VEvent
makeVEvent uid dtstamp event =
  VEvent { veDTStamp = ICal.DTStamp dtstamp def,
           veUID = ICal.UID (fromStrict uid) def,
           veClass = def,
           veDTStart = Just $ ICal.DTStartDate
                       (ICal.Date $ fst $ eventWhen $ event)
                       def,
           veCreated = Nothing,
           veDescription = Nothing,
           veGeo = Nothing,
           veLastMod = Nothing,
           veLocation = fmap makeLocation $ eventWhere event,
           veOrganizer = Nothing,
           vePriority = def,
           veSeq = def,
           veStatus = Nothing,
           veSummary = Just $ makeSummary $ eventName event,
           veTransp = def,
           veUrl = getEventURL event,
           veRecurId = Nothing,
           veRRule = mempty,
           veDTEndDuration = fmap Left $ getDTEnd event,
           veAttach = mempty,
           veAttendee = mempty,
           veCategories = mempty,
           veComment = mempty,
           veContact = mempty,
           veExDate = mempty,
           veRStatus = mempty,
           veRelated = mempty,
           veResources = mempty,
           veRDate = mempty,
           veAlarms = mempty,
           veOther = mempty
         }

makeSummary :: Text -> Summary
makeSummary body = Summary { summaryValue = fromStrict body,
                             summaryAltRep = Nothing,
                             summaryLanguage = Nothing,
                             summaryOther = def
                           }

makeLocation :: Text -> Location
makeLocation body = Location { locationValue = fromStrict body,
                               locationAltRep = Nothing,
                               locationLanguage = Nothing,
                               locationOther = def
                             }

getEventURL :: Event -> Maybe ICal.URL
getEventURL = fmap toICalURL . join . fmap (parseURI . unpack) . eventURI where
  toICalURL uri = ICal.URL uri def

getDTEnd :: Event -> Maybe ICal.DTEnd
getDTEnd ev = if start == end
              then Nothing
              else Just $ ICal.DTEndDate (ICal.Date end) def
  where
    start = fst $ eventWhen $ ev
    end = snd $ eventWhen $ ev
