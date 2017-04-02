-- |
-- Module: App.Scrape.ICal.Parse
-- Description: Parser of web pages etc. for Event data.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module App.Scrape.ICal.Parse
       ( scrapeEventChecker,
         Event(..),
         URIText,
         ErrorMsg,
         ParseResult(..)
       ) where

import Control.Applicative (many, (<$>), (<*>), (<*), (*>), (<|>), optional)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Time (Day, fromGregorian)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as P (decimal)
import Text.Megaparsec.Text (Parser)

type ErrorMsg = String

type URIText = Text

data Event =
  Event { eventName :: Text,
          eventWhen :: (Day, Day), -- ^ (inclusive start day, inclusive end day)
          eventWhere :: Maybe Text,
          eventURI :: Maybe URIText
        } deriving (Show,Eq,Ord)

data ParseResult a = ParseSuccess a
                   | ParseFailure ErrorMsg
                   | ParseMissing
                   deriving (Show,Eq,Ord)

toParseResult :: Show e => Either e (Maybe a) -> ParseResult a
toParseResult (Left err) = ParseFailure $ show err
toParseResult (Right Nothing) = ParseMissing
toParseResult (Right (Just a)) = ParseSuccess a

-- | Scraper for http://event-checker.blog.so-net.ne.jp/
scrapeEventChecker :: String -> Text -> ParseResult Event
scrapeEventChecker filename input = toParseResult $ P.runParser p filename input where
  p = skipTill ((P.eof >> pure Nothing) <|> (Just <$> parserEvent))

parserEvent :: Parser Event
parserEvent = do
  P.string "▼イベント概要" >> br >> P.space
  name <- textTill br
  P.space
  dates <- parserDates
  P.space >> br >> P.space
  place <- textTill br
  muri <- skipTill (P.try (endSummary >> pure Nothing) <|> (Just <$> P.try parserLink))
  return $ Event { eventName = name,
                   eventWhen = dates,
                   eventWhere = Just place,
                   eventURI = muri
                 }

textTill :: Parser e -> Parser Text
textTill end = fmap pack $ P.manyTill P.anyChar end

skipTill :: Parser e -> Parser e
skipTill end = do
  void $ P.manyTill P.anyChar $ P.lookAhead end
  end

br :: Parser ()
br = do
  void $P.string "<"
  P.space
  void $ P.string "br"
  P.space
  void $ P.string "/>"
  return ()

endSummary :: Parser ()
endSummary = void $ P.string "<a name=\"more\"></a>"

parserDates :: Parser (Day, Day)
parserDates = do
  start_y <- decimalWith "年"
  start <- fromGregorian start_y
           <$> decimalWith "月"
           <*> decimalWith "日"
  mend <- optional $ (P.string "-" *> parserEnd start_y)
  return (start, maybe start id mend)
  where
    decimalWith postfix = decimal <* P.string postfix
    parserEnd def_year = fromGregorian
                         <$> (P.try (decimalWith "年") <|> pure def_year)
                         <*> decimalWith "月"
                         <*> decimalWith "日"

decimal :: Num a => Parser a
decimal = fmap fromInteger P.decimal

parserLink :: Parser URIText
parserLink = do
  void $ P.string "<"
  P.space
  void $ P.string "a"
  void $ P.manyTill (P.noneOf ">") (P.string "href=\"")
  uri <- textTill $ P.char '"'
  void $ textTill $ P.char '>'
  void $ P.string "公式サイト</a>"
  return uri
