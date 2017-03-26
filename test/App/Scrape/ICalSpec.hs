module App.Scrape.ICalSpec (main, spec) where

import qualified Data.Text.IO as TIO
import App.Scrape.ICal (scrapeEventChecker)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "scrapeEventChecker" $ do
    it "should scrape date range" $ do
      let file = "test/data/event-checker/american_fes_yokohama.html"
      ret <- fmap (scrapeEventChecker file) $ TIO.readFile file
      print ret
      True `shouldBe` False
