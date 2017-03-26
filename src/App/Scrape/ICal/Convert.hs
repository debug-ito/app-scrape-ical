-- |
-- Module: App.Scrape.ICal.Convert
-- Description: Covert events into ICal data structure.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Scrape.ICal.Convert
       ( toVEvent
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Data.Default.Class (Default(def))
import Data.Monoid (mempty)
import Data.Text (Text, unpack)
import Data.Text.Lazy (fromStrict)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID (nextRandom)
import App.Scrape.ICal.Parse (Event(..))
import Network.URI (parseURI)
import Text.ICalendar (VEvent(..), Summary(..), Location(..))
import qualified Text.ICalendar as ICal

-- | Create 'VEvent' from 'Event'. The UID of 'VEvent' is
-- auto-generated. The DTSTAMP of 'VEvent' is set to the current
-- system time.
toVEvent :: Event -> IO VEvent
toVEvent event = makeVEvent <$> generateUID <*> getCurrentTime <*> pure event

generateUID :: IO Text
generateUID = fmap UUID.toText $ UUID.nextRandom

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
