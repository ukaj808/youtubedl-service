{-# language OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LAZY
import Control.Monad.IO.Class (liftIO)
import Web.Twain
import Types
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectory)
import Control.Concurrent (forkIO)
import System.Process (readProcess)
import System.Directory (listDirectory)

main :: IO ()
main = do
  run 8003 $
    foldr ($) (notFound missing) routes

routes :: [Middleware]
routes =
  [ get "/api/v1/:id" getAudio
  , post "/api/v1" postDownload
  ]

missing :: ResponderM a
missing = send $ html "Not found..."

getAudio :: ResponderM a
getAudio = do
  id <- param "id"
  filePath <- liftIO $ listDirectory id
  audioFile <- liftIO $ LAZY.readFile (id ++ "/" ++ (head filePath))
  send $ raw status200 [(hContentType, "audio/mpeg; charset=utf-8")] audioFile

postDownload :: ResponderM a
postDownload = do
  body <- fromBody
  let _ = body :: DownloadReqBody
  response <- liftIO $ processReq body
  send $ text $ T.pack response


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
  publish id result

publish :: DownloadID -> Result -> IO ()
publish id result = do
  print $ id ++ " " ++ (show result)

buildOutput :: DownloadID -> OutputFile
buildOutput id = "./" ++ id ++ "/%(title)s.%(ext)s"

defaultArgs :: URL -> DownloadID -> [String]
defaultArgs url id = buildArgs url (buildOutput id) "mp3" "./.ffmpeg"

buildArgs :: URL -> OutputFile -> AudioFormat -> FfmpegLocation -> [String]
buildArgs url output format loc =
  ["-x", "-o", output, "--audio-format", format, "--ffmpeg-location", loc, url]

-- todo
mapResult :: StdOut -> Result
mapResult stdout = Success
