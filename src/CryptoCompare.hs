{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A haskell wrapper for the cryptocompare API, a source of information and pricing of different crypto currencies
--
-- > module Main (main) where
-- >
-- > import CryptoCompare
-- >
-- > main :: IO ()
-- > main = do
-- >   coinList <- fetchCoinList
-- >   either print (print . length) coinList
-- >   either print (print . head) coinList
-- >   priceResp <- fetchCurrentPrice "BTC" ["USD", "EUR", "BTC"]
-- >   print priceResp
-- >   priceHistResp <- fetchDailyPriceHistory "BTC" "USD" 300
-- >   print priceHistResp
-- >   snapshotResp <- fetchCoinSnapshot "BTC" "USD"
-- >   print snapshotResp
--
module CryptoCompare
  ( fetchCoinList
  , fetchCurrentPrice
  , fetchDailyPriceHistory
  , fetchCoinSnapshot
  , CoinDetails(..)
  , PriceHistoryResponse(..)
  , PriceHistoryResponseData(..)
  , CoinSnapshot(..)
  , AggregatedSnapshot(..)
  , PriceResponse(..)
  ) where

import qualified Control.Exception      as E
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

-- | High level information about each coin.
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
  snapshotFromSym :: String,
  snapshotToSym   :: String
}

instance ToQueryString CoinSnapshotRequest where
  toQueryString req =
    printf
      "?fsym=%s&tsym=%s"
      (snapshotFromSym req)
      (snapshotToSym req)

-- | Response containing more detailed meta information about a coin, as well as
-- aggregated pricing information
data CoinSnapshotResponse = CoinSnapshotResponse
  { snapshotResponseMessage :: String
  , snapshotResponseType    :: Integer
  , snapshot                :: CoinSnapshot
  } deriving (Show)

instance FromJSON CoinSnapshotResponse where
  parseJSON (Object x) =
    CoinSnapshotResponse <$> x .: "Message" <*> x .: "Type" <*>
    x .: "Data"
  parseJSON _ = error "expected an object"

-- | High level data about a particular coin
data CoinSnapshot = CoinSnapshot
  { snapshotAlgorithm      :: String
  , snapshotProofType      :: String
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
  , low24Hour      :: Float
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
newtype PriceResponse =
  PriceResponse (Map String Float)
  deriving (Show, Generic)
instance FromJSON PriceResponse

-- | Get the price history of a coin (daily)
data PriceHistoryRequest = PriceHistoryRequest {
  -- | coin symbol
  historyFromSym :: String,
  -- | display in currency
  historyToSym   :: String,
  -- | most recent timestamp in returned result
  toTimestamp    :: Maybe UTCTime,
  -- | days to go back
  limit          :: Maybe Integer
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
      (historyFromSym req)
      (historyToSym req)
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

showError :: HttpException -> Either String a
showError = Left . show

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
--
-- @
-- do
--   coinList <- fetchCoinList
--   either print (print . length) coinList
--   either print (print . head) coinList
-- @
--
fetchCoinList :: (MonadIO m) => m (Either String [CoinDetails])
fetchCoinList =
  liftIO $
  E.catch
    (do r <- httpJSON "https://www.cryptocompare.com/api/data/coinlist/"
        return . Right . coins $ getResponseBody r)
    (return . showError)

-- | For a given coin, get a daily history of the coin's price
--
-- > do
-- >   priceHistResp <- fetchDailyPriceHistory "BTC" "USD" 300
-- >   print priceHistResp
--
fetchDailyPriceHistory ::
     (MonadIO m)
  => String -- ^ Coin symbol (`BTC`, `ETH`, etc)
  -> String -- ^ Currency symbol to display prices in (`USD`, `EUR`, etc)
  -> Integer -- ^ Days of history to return (Max 2000)
  -> m (Either String PriceHistoryResponse) -- ^ Either an error or response data
fetchDailyPriceHistory coinSymbol priceCurrency days =
  liftIO $
  E.catch
    (do priceHistReq <-
          return . parseRequest $
          "https://min-api.cryptocompare.com/data/histoday" ++
          toQueryString
            (priceHistReqDefault
             { historyFromSym = coinSymbol
             , historyToSym = priceCurrency
             , limit = Just days
             } :: PriceHistoryRequest)
        r <- httpJSON <$> priceHistReq
        Right . getResponseBody <$> r)
    (return . showError)

-- | For a given coin, get the current price
--
--
-- > do
-- >   priceResp <- fetchCurrentPrice "BTC" ["USD", "EUR", "BTC"]
-- >   print priceResp
--
fetchCurrentPrice ::
     MonadIO m
  => String -- ^ Coin symbol (`BTC`, `ETH`, etc)
  -> [String] -- ^ Currency symbol(s) to display prices in. Eg [`USD`, `EUR`, ...]
  -> m (Either String PriceResponse) -- ^ Either an error or response data
fetchCurrentPrice coinSymbol priceSymbols =
  liftIO $
  E.catch
    (do priceReq <-
          return . parseRequest $
          "https://min-api.cryptocompare.com/data/price" ++
          toQueryString (PriceRequest coinSymbol priceSymbols)
        r <- httpJSON <$> priceReq
        Right . getResponseBody <$> r)
    (return . showError)

-- | Fetch details about a particular coin
--
-- > do
-- >  snapshotResp <- fetchCoinSnapshot "BTC" "USD"
-- >  print snapshotResp
--
fetchCoinSnapshot ::
     MonadIO m
  => String -- ^ Coin symbol (`BTC`, `ETH`, etc)
  -> String -- ^ Currency symbol(s) to display prices in (`USD`, `EUR`, etc)
  -> m (Either String CoinSnapshot) -- ^ Either an error or response data
fetchCoinSnapshot fSym tSym =
  liftIO $
  E.catch
    (do snapshotReq <-
          return . parseRequest $
          "https://www.cryptocompare.com/api/data/coinsnapshot" ++
          toQueryString (CoinSnapshotRequest fSym tSym)
        r <- httpJSON <$> snapshotReq
        Right . snapshot . getResponseBody <$> r)
    (return . showError)
