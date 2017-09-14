{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Hasql.Simple where

import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Int
import Data.Time
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

req :: DbEncode v => (a -> v) -> E.Params a
req get = get ->. dbEnc

opt :: DbEncode v => (a -> Maybe v) -> E.Params a
opt get = get ->? dbEnc

(->.) :: (a -> v) -> E.Value v -> E.Params a
get ->. ser =
    contramap get (E.value ser)

(->?) :: (a -> Maybe v) -> E.Value v -> E.Params a
get ->? ser =
    contramap get (E.nullableValue ser)

class RowUnpacker r where
    unpackRows :: forall e. D.Row e -> D.Result (r e)

instance RowUnpacker V.Vector where
    unpackRows = D.rowsVector

instance RowUnpacker Maybe where
    unpackRows = D.maybeRow

type family DbRepr t

class DbEncode t where
    packVal :: t -> DbRepr t
    dbEnc :: E.Value t

    default packVal :: t -> t
    packVal = id

    default dbEnc :: DbEncode (DbRepr t) => E.Value t
    dbEnc = contramap packVal dbEnc

class DbDecode t where
    unpackVal :: DbRepr t -> t
    dbDec :: D.Value t

    default unpackVal :: t -> t
    unpackVal = id

    default dbDec :: DbDecode (DbRepr t) => D.Value t
    dbDec = unpackVal <$> dbDec

type instance DbRepr Bool = Bool
instance DbEncode Bool where
    dbEnc = E.bool

instance DbDecode Bool where
    dbDec = D.bool

type instance DbRepr T.Text = T.Text
instance DbEncode T.Text where
    dbEnc = E.text

instance DbDecode T.Text where
    dbDec = D.text

type instance DbRepr Int64 = Int64
instance DbEncode Int64 where
    dbEnc = E.int8

instance DbDecode Int64 where
    dbDec = D.int8

type instance DbRepr Double = Double
instance DbEncode Double where
    dbEnc = E.float8

instance DbDecode Double where
    dbDec = D.float8

type instance DbRepr BS.ByteString = BS.ByteString
instance DbEncode BS.ByteString where
    dbEnc = E.bytea

instance DbDecode BS.ByteString where
    dbDec = D.bytea

type instance DbRepr UTCTime = UTCTime
instance DbEncode UTCTime where
    dbEnc = E.timestamptz

instance DbDecode UTCTime where
    dbDec = D.timestamptz

type instance DbRepr Day = Day
instance DbEncode Day where
    dbEnc = E.date

instance DbDecode Day where
    dbDec = D.date

type instance DbRepr (V.Vector a) = V.Vector a -- this is a bit cheated here ...
instance DbEncode a => DbEncode (V.Vector a) where
    dbEnc = E.array (E.arrayDimension V.foldl' (E.arrayValue dbEnc))

instance DbDecode a => DbDecode (V.Vector a) where
    dbDec = D.array (D.arrayDimension V.replicateM (D.arrayValue dbDec))

type instance DbRepr (HM.HashMap T.Text a) = HM.HashMap T.Text a -- this is a bit cheated here ...
instance ToJSON a => DbEncode (HM.HashMap T.Text a) where
    dbEnc = jsonbE

instance FromJSON a => DbDecode (HM.HashMap T.Text a) where
    dbDec = jsonbD

jsonbE :: ToJSON a => E.Value a
jsonbE = contramap toJSON E.jsonb

jsonbD :: FromJSON a => D.Value a
jsonbD = D.jsonbBytes (first T.pack . eitherDecodeStrict')

jsonbD' :: (Value -> Parser a) -> D.Value a
jsonbD' parser =
    D.jsonbBytes $ \raw ->
    do v <-
           first (\pError -> T.pack (pError ++ ": Value was: " ++ show raw)) $
           eitherDecodeStrict' raw
       first (\pError -> T.pack (pError ++ ": Value was: " ++ show v)) $ parseEither parser v

jsonVec :: (Value -> Parser a) -> D.Value (V.Vector a)
jsonVec parser =
    D.array (D.arrayDimension V.replicateM $ D.arrayValue $ jsonD' parser)

jsonD' :: (Value -> Parser a) -> D.Value a
jsonD' parser =
    D.jsonBytes $ \raw ->
    do v <-
           first (\pError -> T.pack (pError ++ ": Value was: " ++ show raw)) $
           eitherDecodeStrict' raw
       first (\pError -> T.pack (pError ++ ": Value was: " ++ show v)) $ parseEither parser v
