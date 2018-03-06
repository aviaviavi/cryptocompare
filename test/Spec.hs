{-# LANGUAGE ScopedTypeVariables #-}

import           CryptoCompare
import           Data.Either
import qualified Data.Either.Utils as U
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "Testing the library against the live CryptoCompare API" $
    it "performs basic example fetches" $ do
  coinList <- fetchCoinList
  coinList `shouldSatisfy` isRight
  U.fromRight coinList `shouldSatisfy` (\x -> length x > 1000)
  priceResp <- fetchCurrentPrice "BTC" ["USD", "EUR", "BTC"]
  priceResp `shouldSatisfy` isRight
  priceHistResp <- fetchDailyPriceHistory "BTC" "USD" 300
  priceHistResp `shouldSatisfy` isRight
  snapshotResp <- fetchCoinSnapshot "BTC" "USD"
  snapshotResp `shouldSatisfy` isRight


