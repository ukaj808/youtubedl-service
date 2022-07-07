{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Types
import           Web.Twain

import           Control.Concurrent       (forkIO)
import           Control.Monad.IO.Class   (liftIO)
import           Data.UUID                (toString)
import           Data.UUID.V4             (nextRandom)
import           Network.Wai.Handler.Warp (run)
import           System.Directory         (listDirectory,
                                           removeDirectoryRecursive, removeFile)
import           System.Process           (readProcess)

import qualified Data.ByteString.Char8    as REG
import qualified Data.ByteString.Lazy     as LAZY
import qualified Data.Text                as T
import qualified Database.Redis           as R

main :: IO ()
main = do
  run 8003 $
    foldr ($) (notFound missing) routes

routes :: [Middleware]
routes =
  [ get "/api/v1/:id" getAudio
  , post "/api/v1" postDownload
  , delete "/api/v1/:id" deleteAudio
  ]

missing :: ResponderM a
missing = send $ html "Not found..."

getAudio :: ResponderM a
getAudio = do
  id <- param "id"
  let dir = pathToAudio id
  fileName <- liftIO $ listDirectory dir
  audioFile <- liftIO $ LAZY.readFile (dir ++ "/" ++ (head fileName))
  send $ raw status200 [(hContentType, "audio/mpeg; charset=utf-8")] audioFile

postDownload :: ResponderM a
postDownload = do
  body <- fromBody
  let _ = body :: DownloadReqBody
  response <- liftIO $ processReq body
  send $ status status202 $ text $ T.pack response

deleteAudio :: ResponderM a
deleteAudio = do
  id <- param "id"
  let dir = pathToAudio id
  liftIO $ removeDirectoryRecursive dir
  send $ status status204 $ text "Delete Succesful"

processReq :: DownloadReqBody -> IO String
processReq req = do
  id <- nextRandom
  let reqUrl = T.unpack $ (url req)
  let stringId = toString id
  forkIO $ download reqUrl stringId
  return stringId

download :: URL -> DownloadID -> IO ()
download url id = do
  stdOut <- readProcess "./.yt-dlp/linux/yt-dlp_linux" (defaultArgs url id) []
  let result = mapResult stdOut
  publishDownloadResult id result

publishDownloadResult :: DownloadID -> Result -> IO ()
publishDownloadResult id result = do
  let eventBody | result == Success = successEventBody id
                | otherwise = failureEventBody id
  conn <- R.checkedConnect redisInfo
  R.runRedis conn $ do
    R.publish resultChannel eventBody
  print $ id ++ " " ++ (show result)

mapResult :: StdOut -> Result
mapResult stdout = Success

buildOutput :: DownloadID -> OutputFile
buildOutput id = (pathToAudio id) ++ "/%(title)s.%(ext)s"

defaultArgs :: URL -> DownloadID -> [String]
defaultArgs url id = buildArgs url (buildOutput id) "mp3" "./.ffmpeg"

buildArgs :: URL -> OutputFile -> AudioFormat -> FfmpegLocation -> [String]
buildArgs url output format loc =
  ["-x", "-o", output, "--audio-format", format, "--ffmpeg-location", loc, url]

pathToAudio :: DownloadID -> FilePath
pathToAudio id =  "./.downloads/" ++ id

redisInfo :: R.ConnectInfo
redisInfo = R.ConnInfo
  {
      R.connectHost           = 
        "redis-16666.c284.us-east1-2.gce.cloud.redislabs.com",
      R.connectPort           = R.PortNumber 16666,
      R.connectAuth           = Just "tlgQw0WugrW3xqoBlEe9VJqJk86v4Dy1",
      R.connectDatabase       = 0,
      R.connectMaxConnections = 50,
      R.connectMaxIdleTime    = 30,
      R.connectTimeout        = Nothing,
      R.connectTLSParams      = Nothing
  }

resultChannel :: REG.ByteString
resultChannel = "private.ytdl.result"

successEventBody :: DownloadID -> REG.ByteString
successEventBody id = resultEventBody id "SUCCESS"

failureEventBody :: DownloadID -> REG.ByteString
failureEventBody id = resultEventBody id "FAILURE"

resultEventBody :: DownloadID -> String -> REG.ByteString
resultEventBody id msg =
  REG.pack $ "{\"id\":\"" ++ id ++ "\", \"status\":\"" ++ msg ++ "\"}"
