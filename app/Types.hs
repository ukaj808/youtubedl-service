{-# language OverloadedStrings #-}

module Types where

import qualified Data.Text as T

import Data.Aeson
  (FromJSON, ToJSON, parseJSON, withObject,
   toJSON, toEncoding, object, pairs,
   (.:), (.=))

data DownloadReqBody = DownloadReqBody { url :: T.Text }
instance FromJSON DownloadReqBody where
  parseJSON = withObject "DownloadReqBody" $ \v -> DownloadReqBody
    <$> v .: "url"
instance ToJSON DownloadReqBody where
  toJSON (DownloadReqBody url) = object ["url" .= url]
  toEncoding (DownloadReqBody url) = pairs ("url" .= url)

data Result = Success | Failure
  deriving (Show)
type URL = String
type FfmpegLocation = String
type AudioFormat = String
type DownloadID = String
type OutputFile = String
type StdOut = String
