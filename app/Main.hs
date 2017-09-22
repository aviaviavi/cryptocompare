module Main (main) where

import CryptoCompare

main :: IO ()
main = do
  coinList <- fetchCoinList
  either print (print . length) coinList
  either print (print . head) coinList
  priceResp <- fetchCurrentPrice "BTC" ["USD", "EUR", "BTC"]
  print priceResp
  -- priceHistResp <- fetchDailyPriceHistory "BTC" "USD" 300
  -- print priceHistResp
  -- snapshotResp <- fetchCoinSnapshot "BTC" "USD"
  -- print snapshotResp
