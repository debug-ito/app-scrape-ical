module App.Scrape.ICalSpec (main, spec) where

import qualified Data.Text.IO as TIO
import Data.Time (Day, fromGregorian)
import App.Scrape.ICal
  ( scrapeEventChecker,
    Event(..), ParseResult(..)
  )

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "scrapeEventChecker" $ do
    it "should scrape date range" $ do
      let file = "test/data/event-checker/american_fes_yokohama.html"
      ret <- fmap (scrapeEventChecker file) $ TIO.readFile file
      ret `shouldBe` ParseSuccess Event { eventName = "第10回アフリカンフェスティバルよこはま2017",
                                          eventWhen = (day 2017 3 24, day 2017 3 26),
                                          eventWhere = Just "横浜赤レンガ倉庫１号館",
                                          eventURI = Just "http://africanfestyokohama.com/"
                                        }

day :: Integer -> Int -> Int -> Day
day = fromGregorian
