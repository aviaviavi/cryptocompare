{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib
    ( fetchCoinList,
      fetchCurrentPrice,
      fetchDailyPriceHistory,

      CoinListResponse(..),
      CoinDetails
    ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict
import           Data.List
import           Data.Map               (Map)
import           Data.Maybe
import           Data.Time.Clock
import           GHC.Generics
import           Network.HTTP.Simple
import           Text.Printf

class ToQueryString a where
  toQueryString :: a -> String

data CoinListResponse = CoinListResponse {
    response     :: String,
    message      :: String,
    responseType :: Integer,
    coins        :: [CoinDetails]
} deriving (Show, Generic)

data CoinDetails = CoinDetails {
    _key      :: String,
    id        :: String,
    url       :: String,
    imageUrl  :: Maybe String,
    name      :: String,
    coinName  :: String,
    fullName  :: String,
    algorithm :: String,
    proofType :: String,
    sortOrder :: String
} deriving (Show, Generic)

data PriceRequest = PriceRequest {
    fromSym :: String,
    toSyms  :: [String]
}

instance ToQueryString PriceRequest where
  toQueryString req =
    printf
      "?fsym=%s&tsyms=%s"
      (fromSym (req :: PriceRequest))
      (intercalate "," $ toSyms (req :: PriceRequest))

newtype PriceResponse = PriceResponse (Map String Float) deriving (Show, Generic)
instance FromJSON PriceResponse

-- in days
data PriceHistoryRequest = PriceHistoryRequest {
    fromSym     :: String,
    toSym       :: String,
    toTimestamp :: Maybe UTCTime,
    limit       :: Maybe Integer
}
priceHistReqDefault :: PriceHistoryRequest
priceHistReqDefault = PriceHistoryRequest "" [] Nothing Nothing

data PriceHistoryResponse = PriceHistoryResponse {
    responseData :: [PriceHistoryResponseData],
    timeTo       :: Maybe Integer,
    timeFrom     :: Maybe Integer
} deriving (Show, Generic)
instance FromJSON PriceHistoryResponse where
  parseJSON (Object x) =
    PriceHistoryResponse <$> x .: "Data" <*> x .:? "TimeTo" <*> x .:? "TimeFrom"
  parseJSON _ = error "expected an object"

data PriceHistoryResponseData = PriceHistoryResponseData {
    time       :: Float,
    open       :: Float,
    high       :: Float,
    low        :: Float,
    close      :: Float,
    volumefrom :: Float,
    volumeto   :: Float
} deriving (Show, Generic)
instance FromJSON PriceHistoryResponseData

instance ToQueryString PriceHistoryRequest where
  toQueryString req =
    printf
      "?fsym=%s&tsym=%s&limit=%s"
      (fromSym (req :: PriceHistoryRequest))
      (toSym (req :: PriceHistoryRequest))
      (fromMaybe "" (show <$> limit req))

instance FromJSON CoinListResponse where
  parseJSON (Object x) =
    CoinListResponse <$> x .: "Response" <*> x .: "Message" <*> x .: "Type" <*>
    x .: "Data"
  parseJSON _ = error "expected an object"

-- Aeson claims this definition overlaps with the internal
-- instance of FromJSON [a], so the overlap pragma seems to fix that
instance {-# OVERLAPS #-} FromJSON [CoinDetails] where
    parseJSON x =
      parseJSON x >>= mapM parseCointListResponseData . toList

parseCointListResponseData :: (String, Value) -> Parser CoinDetails
parseCointListResponseData (i, v) =
  withObject
    "entry body"
    (\o ->
       CoinDetails i <$> o .: "Id" <*> o .: "Url" <*> o .:? "ImageUrl" <*>
       o .: "Name" <*>
       o .: "CoinName" <*>
       o .: "FullName" <*>
       o .: "Algorithm" <*>
       o .: "ProofType" <*>
       o .: "SortOrder")
    v

fetchCoinList :: MonadIO m => m [CoinDetails]
fetchCoinList = do
  r <- httpJSON "https://www.cryptocompare.com/api/data/coinlist/"
  return . coins $ getResponseBody r

fetchDailyPriceHistory ::
     (MonadIO m, MonadThrow m)
  => String
  -> String
  -> Integer
  -> m PriceHistoryResponse
fetchDailyPriceHistory coinSymbol priceCurrency days = do
  priceHistReq <-
    return . parseRequest $
    "https://min-api.cryptocompare.com/data/histoday" ++
    toQueryString
      (priceHistReqDefault
       {fromSym = coinSymbol, toSym = priceCurrency, limit = Just days} :: PriceHistoryRequest)
  r <- httpJSON <$> priceHistReq
  getResponseBody <$> r

fetchCurrentPrice
    :: (MonadIO m, MonadThrow m) => String -> [String] -> m PriceResponse
fetchCurrentPrice coinSymbol priceSymbols = do
  priceReq <-
    return . parseRequest $
    "https://min-api.cryptocompare.com/data/price" ++
    (toQueryString $ PriceRequest coinSymbol priceSymbols)
  r <- httpJSON <$> priceReq
  getResponseBody <$> r
