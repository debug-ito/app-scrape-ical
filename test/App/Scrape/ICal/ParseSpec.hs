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
      ret <- loadAndParse' "hosokawa_delta_header.html"
      ret `shouldBe` ParseSuccess Event { eventName = "秋の紅葉ライトアップ ～ひごあかり～",
                                          eventWhen = (day 2019 11 23, day 2019 12 1),
                                          eventWhere = Just "肥後細川庭園（旧：新江戸川公園）",
                                          eventURI = Just "https://www.higo-hosokawa.jp/"
                                        }
    it "should scrape single date" $ do
      ret <- loadAndParse' "ippudo_single_day.html"
      ret `shouldBe` ParseSuccess Event { eventName = "一風堂「勤労感謝玉(替玉一玉)」プレゼントキャンペーン",
                                          eventWhen = (day 2019 11 23, day 2019 11 23),
                                          eventWhere = Just "一風堂 国内全店舗",
                                          eventURI = Just "https://www.ippudo.com/"
                                        }
    it "should return ParseMissing if there is no event summary in the page" $ do
      ret <- loadAndParse' "kfc_lunch.html"
      ret `shouldBe` ParseMissing
    it "should scrape an event over year boundary" $ do
      ret <- loadAndParse' "enoshima_2019_2020.html"
      ret `shouldBe` ParseSuccess Event { eventName = "湘南の宝石 ～江の島を彩る光と色の祭典～",
                                          eventWhen = (day 2019 11 23, day 2020 2 16),
                                          eventWhere = Just "江の島サムエル・コッキング苑、他",
                                          eventURI = Just "https://enoshima-seacandle.com/event/shonannohoseki2018-2019/"
                                        }

day :: Integer -> Int -> Int -> Day
day = fromGregorian

loadAndParse :: (String -> Text -> ParseResult a) -> String -> String -> IO (ParseResult a)
loadAndParse parser dir filename = let file = dir ++ "/" ++ filename
                                   in fmap (parser file) $ TIO.readFile file
