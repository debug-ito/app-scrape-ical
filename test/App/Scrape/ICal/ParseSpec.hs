module App.Scrape.ICal.ParseSpec (main, spec) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time (Day, fromGregorian)
import App.Scrape.ICal.Parse
  ( scrapeEventChecker,
    Event(..), ParseResult(..)
  )

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "scrapeEventChecker" $ do
    let loadAndParse' = loadAndParse scrapeEventChecker "test/data/event-checker"
    it "should scrape date range" $ do
      ret <- loadAndParse' "american_fes_yokohama.html"
      ret `shouldBe` ParseSuccess Event { eventName = "第10回アフリカンフェスティバルよこはま2017",
                                          eventWhen = (day 2017 3 24, day 2017 3 26),
                                          eventWhere = Just "横浜赤レンガ倉庫１号館",
                                          eventURI = Just "http://africanfestyokohama.com/"
                                        }
    it "should scrape single date" $ do
      ret <- loadAndParse' "sakura_matsuri.html"
      ret `shouldBe` ParseSuccess Event { eventName = "新宿花園ゴールデン街 桜まつり2017",
                                          eventWhen = (day 2017 4 16, day 2017 4 16),
                                          eventWhere = Just "新宿ゴールデン街",
                                          eventURI = Just "http://golden-gai.tokyo/sakura2017/"
                                        }

day :: Integer -> Int -> Int -> Day
day = fromGregorian

loadAndParse :: (String -> Text -> ParseResult a) -> String -> String -> IO (ParseResult a)
loadAndParse parser dir filename = let file = dir ++ "/" ++ filename
                                   in fmap (parser file) $ TIO.readFile file
