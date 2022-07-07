{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson (FromJSON, ToJSON, object, pairs, parseJSON,
                             toEncoding, toJSON, withObject, (.:), (.=))
import qualified Data.Text  as T

data DownloadReqBody = DownloadReqBody { url :: T.Text }
instance FromJSON DownloadReqBody where
  parseJSON = withObject "DownloadReqBody" $ \v -> DownloadReqBody
    <$> v .: "url"
instance ToJSON DownloadReqBody where
  toJSON (DownloadReqBody url) = object ["url" .= url]
  toEncoding (DownloadReqBody url) = pairs ("url" .= url)

data Result         = Success | Failure
  deriving (Show, Eq)

type URL            = String
type FfmpegLocation = String
type AudioFormat    = String
type DownloadID     = String
type OutputFile     = String
type StdOut         = String
