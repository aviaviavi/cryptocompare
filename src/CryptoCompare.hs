{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | A haskell wrapper for the cryptocompare API, a source of information and pricing of different crypto currencies
module CryptoCompare
  ( fetchCoinList
  , fetchCurrentPrice
  , fetchDailyPriceHistory
  , fetchCoinSnapshot
  , CoinListResponse(..)
  , CoinDetails(..)
  , PriceHistoryResponse(..)
  , PriceHistoryResponseData(..)
  , CoinSnapshotResponse(..)
  , CoinSnapshot(..)
  , AggregatedSnapshot(..)
  , PriceResponse(..)
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

-- | Typeclass for datatypes from which url query parameters can be generated
class ToQueryString a where
  toQueryString :: a -> String

-- | List of all known coins, and some basic info for them
data CoinListResponse = CoinListResponse {
  response     :: String,
  message      :: String,
  responseType :: Integer,
  coins        :: [CoinDetails]
} deriving (Show, Generic)

-- | High level information about each coin, part of
-- 'CoinListResponse'
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

-- | Request the current price of a particular cryptocurrency
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

-- TODO - get rid of repeated code

-- | Request a more detailed view of a coin's information
data CoinSnapshotRequest = CoinSnapshotRequest {
  fromSym :: String,
  toSym   :: String
}

instance ToQueryString CoinSnapshotRequest where
  toQueryString req =
    printf
      "?fsym=%s&tsym=%s"
      (fromSym (req :: CoinSnapshotRequest))
      (toSym (req :: CoinSnapshotRequest))

-- | Response containing more detailed meta information about a coin, as well as
-- aggregated pricing information
data CoinSnapshotResponse = CoinSnapshotResponse
  { response     :: String
  , message      :: String
  , responseType :: Integer
  , snapshot     :: CoinSnapshot
  } deriving (Show)

instance FromJSON CoinSnapshotResponse where
  parseJSON (Object x) =
    CoinSnapshotResponse <$> x .: "Response" <*> x .: "Message" <*> x .: "Type" <*>
    x .: "Data"
  parseJSON _ = error "expected an object"

-- | High level data about a particular coin
data CoinSnapshot = CoinSnapshot
  { algorithm              :: String
  , proofType              :: String
  , blockNumber            :: Integer
  , netHashesPerSecond     :: Float
  , totalCoinsMined        :: Float
  , blockReward            :: Float
  , aggregatedSnapshotData :: AggregatedSnapshot
  } deriving (Show)

instance FromJSON CoinSnapshot where
  parseJSON (Object x) =
    CoinSnapshot <$> x .: "Algorithm" <*> x .: "ProofType" <*>
    x .: "BlockNumber" <*>
    x .: "NetHashesPerSecond" <*>
    x .: "TotalCoinsMined" <*>
    x .: "BlockReward" <*>
    x .: "AggregatedData"
  parseJSON _ = error "expected an object"

-- | Aggregated data about a particular coin
data AggregatedSnapshot = AggregatedSnapshot
  { market         :: String
  , fromSymbol     :: String
  , toSymbol       :: String
  , flags          :: String
  , price          :: Float
  , lastUpdate     :: Integer
  , lastVolume     :: Float
  , lastVolumeto   :: Float
  , lastTradeId    :: String
  , volume24Hour   :: Float
  , volume24HourTo :: Float
  , open24Hour     :: Float
  , high24Hour     :: Float
  , low24Hour      :: Float -- ^ a description of the getter here!!!!
  , lastMarket     :: String
  } deriving (Show)
instance FromJSON AggregatedSnapshot where
  parseJSON (Object x) =
    AggregatedSnapshot <$> x .: "MARKET" <*> x .: "FROMSYMBOL" <*>
    x .: "TOSYMBOL" <*>
    x .: "FLAGS" <*>
    (read <$> x .: "PRICE") <*>
    (read <$> x .: "LASTUPDATE") <*>
    (read <$> x .: "LASTVOLUME") <*>
    (read <$> x .: "LASTVOLUMETO") <*>
    x .: "LASTTRADEID" <*>
    (read <$> x .: "VOLUME24HOUR") <*>
    (read <$> x .: "VOLUME24HOURTO") <*>
    (read <$> x .: "OPEN24HOUR") <*>
    (read <$> x .: "HIGH24HOUR") <*>
    (read <$> x .: "LOW24HOUR") <*>
    x .: "LASTMARKET"
  parseJSON _ = error "expected an object"

-- | contains pairs of prices: crypto symbol -> (regular currency symbol, price)
data PriceResponse =
  PriceResponse (Map String Float)
  deriving (Show, Generic)
instance FromJSON PriceResponse

-- | Get the price history of a coin (daily)
data PriceHistoryRequest = PriceHistoryRequest {
  -- | coin symbol
  fromSym     :: String,
  -- | display in currency
  toSym       :: String,
  -- | most recent timestamp in returned result
  toTimestamp :: Maybe UTCTime,
  -- | days to go back
  limit       :: Maybe Integer
}
priceHistReqDefault :: PriceHistoryRequest
priceHistReqDefault = PriceHistoryRequest "" [] Nothing Nothing

-- | API response container for daily price history
data PriceHistoryResponse = PriceHistoryResponse {
  responseData :: [PriceHistoryResponseData], -- ^ the actual response, list of price entries
  timeTo       :: Maybe Integer, -- ^ latest price returned
  timeFrom     :: Maybe Integer -- ^ earliest price returned
} deriving (Show, Generic)

instance FromJSON PriceHistoryResponse where
  parseJSON (Object x) =
    PriceHistoryResponse <$> x .: "Data" <*> x .:? "TimeTo" <*> x .:? "TimeFrom"
  parseJSON _ = error "expected an object"

-- | Data for a particular snapshot of a coin's daily price
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
-- TODO - this seems wrong, there's probably a cleaner way to write
-- that doesn't involve extension pragmas
instance {-# OVERLAPS #-} FromJSON [CoinDetails] where
    parseJSON x =
      parseJSON x >>= mapM parseCointListResponseData . toList

-- Since this entry uses keys that we don't know ahead of time, we
-- need to do some special parsing that doesn't require it.
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

-- | Get a list of all of the coins the API is aware of, and high level details
-- about those coins
fetchCoinList :: (MonadIO m, MonadCatch m) => m (Either String [CoinDetails])
fetchCoinList = catchIOError (do
  r <- httpJSON "https://www.cryptocompare.com/api/data/coinlist/"
  return . Right . coins $ getResponseBody r)
  (return . Left . show)

-- | For a given coin, get a daily history of the coin's price
fetchDailyPriceHistory ::
     (MonadIO m, MonadThrow m, MonadCatch m)
  => String
  -> String
  -> Integer
  -> m (Either String PriceHistoryResponse)
fetchDailyPriceHistory coinSymbol priceCurrency days = catchIOError (do
  priceHistReq <-
    return . parseRequest $
    "https://min-api.cryptocompare.com/data/histoday" ++
    toQueryString
      (priceHistReqDefault
       {fromSym = coinSymbol, toSym = priceCurrency, limit = Just days} :: PriceHistoryRequest)
  r <- httpJSON <$> priceHistReq
  Right . getResponseBody <$> r)
  (return . Left . show)

-- | For a given coin, get the current price
fetchCurrentPrice ::
     (MonadIO m, MonadThrow m, MonadCatch m) => String -> [String] -> m (Either String PriceResponse)
fetchCurrentPrice coinSymbol priceSymbols = catchIOError (do
  priceReq <-
    return . parseRequest $
    "https://min-api.cryptocompare.com/data/price" ++
    toQueryString (PriceRequest coinSymbol priceSymbols)
  r <- httpJSON <$> priceReq
  Right . getResponseBody <$> r)
  (return . Left . show)

-- | Fetch details about a particular coin
fetchCoinSnapshot :: (MonadIO m, MonadThrow m, MonadCatch m) => String -> String -> m (Either String CoinSnapshotResponse)
fetchCoinSnapshot fSym tSym = catchIOError (do
  snapshotReq <-
    return . parseRequest $
    "https://www.cryptocompare.com/api/data/coinsnapshot" ++
    toQueryString (CoinSnapshotRequest fSym tSym)
  r <- httpJSON <$> snapshotReq
  Right . getResponseBody <$> r)
  (return . Left . show)
