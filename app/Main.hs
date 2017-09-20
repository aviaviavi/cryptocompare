module Main (main) where

import Lib

main :: IO ()
main = do
  coinList <- fetchCoinList
  print . length $ coinList
  print . head $ coinList
  priceResp <- fetchCurrentPrice "BTC" ["USD", "EUR", "BTC"]
  print priceResp
  priceHistResp <- fetchDailyPriceHistory "BTC" "USD" 300
  print priceHistResp
  snapshotResp <- fetchCoinSnapshot "BTC" "USD"
  print snapshotResp
