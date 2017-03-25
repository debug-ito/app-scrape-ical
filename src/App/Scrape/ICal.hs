-- |
-- Module: App.Scrape.ICal
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module App.Scrape.ICal
       () where

import Control.Applicative (many, (<$>), (<*>), (<*))
import Data.Text (Text, pack)
import Data.Time (Day, fromGregorian)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as P (decimal)
import Text.Megaparsec.Text (Parser)

type ErrorMsg = String

type URIText = Text

data Event =
  Event { eventName :: Text,
          eventWhen :: (Day, Day),
          eventWhere :: Maybe Text,
          eventURI :: Maybe URIText
        } deriving (Show,Eq,Ord)

scrapeEventChecker :: Text -> Either ErrorMsg Event
scrapeEventChecker = undefined

parserEvent :: Parser Event
parserEvent = do
  P.string "▼イベント概要" >> br >> P.space
  dates <- parserDates
  P.space >> br >> P.space
  name <- textTill (P.space >> br)
  P.space
  place <- textTill (P.space >> br)
  undefined

textTill :: Parser e -> Parser Text
textTill end = fmap pack $ P.manyTill P.anyChar end

br :: Parser ()
br = do
  P.string "<"
  P.space
  P.string "br"
  P.space
  P.string "/>"
  return ()

parserDates :: Parser Event
parserDates = do
  start <- fromGregorian
           <$> (decimal <* P.string "年")
           <*> (decimal <* P.string "月")
           <*> (decimal <* P.string "日")
  undefined -- todo. 終了日はない場合もある。あと年は省略されるかも。

decimal :: Num a => Parser a
decimal = fmap fromInteger P.decimal
