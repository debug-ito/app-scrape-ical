-- |
-- Module: App.Scrape.ICal.Convert
-- Description: Covert events into ICal data structure.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Scrape.ICal.Convert
       () where

import Data.Text (Text)
import App.Scrape.ICal.Parse (Event(..))
import Text.ICalendar (VEvent(..))

-- | Create 'VEvent' from 'Event'. The UID of 'VEvent' is
-- auto-generated. The DTSTAMP of 'VEvent' is set to the current
-- system time.
toVEvent :: Event -> IO VEvent
toVEvent event = undefined

