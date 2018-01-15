{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.SyncFile (syncFile) where

import System.IO (FilePath, stderr, withFile, IOMode(AppendMode))
import System.FSNotify (
  Event(Added, Modified, Removed),
  WatchManager,
  watchDir,
  eventPath,
  StopListening
  )
import qualified System.FSNotify
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.FilePath (splitFileName)
import Network.HTTP.Simple (
  httpLBS,
  setRequestMethod, setRequestHost, setRequestPath, setRequestPort, setRequestBody,
  defaultRequest,
  getResponseStatus, getResponseBody
  )
import Network.HTTP.Types.Status (ok200, Status(statusCode))
import Data.IORef (newIORef, readIORef, atomicWriteIORef)
import System.IO.StringLike.PutStrLn (hPutStrLn)
import System.IO.StringLike.PutStr (hPutStr)
import System.IO.StringLike.GetContents (hGetContents)
import System.IO.AtomicFileOps (atomicModifyFile)
import Network.HTTP.Conduit (RequestBody(RequestBodyBS))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.FileLock (withFileLock, SharedExclusive(Exclusive))
import System.Directory (makeAbsolute)
import Data.Semigroup ((<>))

syncFile :: BS.ByteString -> Int -> BS.ByteString -> Int -> FilePath -> IO ()
syncFile hostName hostPort hostPath serverCheckMicroseconds relativeFilePath =
  let
    requestBasics = setRequestHost hostName . setRequestPath hostPath . setRequestPort hostPort $ defaultRequest
  in do
    filePath <- makeAbsolute relativeFilePath
    let lockFilePath = filePath <> ".lock"
    withFile lockFilePath AppendMode (const (pure ())) -- create lock file if it doesn't exist
    (initialContents :: BS.ByteString) <- hGetContents filePath
    currentContentsRef <- newIORef initialContents
    listenToEvents <- newIORef True
    let
      doUpload contents =
        let
          request =
              setRequestMethod "POST"
            . setRequestBody (RequestBodyBS contents)
            $ requestBasics
        in do
          response <- httpLBS request
          let status = getResponseStatus response
          if
            | status == ok200 -> do
                atomicWriteIORef currentContentsRef contents
                note ("Success uploading file")
            | otherwise -> do
                let body = getResponseBody response
                hPutStrLn stderr ("Error uploading file" :: BS.ByteString)
                hPutStr stderr ("HTTP error code: " :: BS.ByteString)
                hPutStrLn stderr (show . statusCode $ status)
                hPutStrLn stderr ("Body:" :: BS.ByteString)
                hPutStrLn stderr body
    let
      uploadFile = withFileLock lockFilePath Exclusive go where
        go _ = do
          newContents <- hGetContents filePath
          doUpload newContents
    let
      downloadFile =
        let
          request =
              setRequestMethod "GET"
            $ requestBasics
          f actualFileContents = do
            currentContents <- readIORef currentContentsRef
            case (actualFileContents == currentContents) of
              -- If there's been no changes to the file
              True -> do
                note ("There has been no changes so checking server")
                response <- httpLBS request
                let status = getResponseStatus response
                let body = BSL.toStrict . getResponseBody $ response
                if
                  | status == ok200 -> do
                      note ("Success downloading file")
                      case (body == currentContents) of
                        True -> do
                          note "Downloaded file is the same as current file so do nothing"
                          pure (pure (), Nothing)
                        False -> do
                          note "Downloaded file is different current file so replace"
                          atomicWriteIORef currentContentsRef body
                          pure (pure (), Just body)
                  | otherwise -> do
                      hPutStrLn stderr ("Error downloading file" :: BS.ByteString)
                      hPutStr stderr ("HTTP error code: " :: BS.ByteString)
                      hPutStrLn stderr (show . statusCode $ status)
                      hPutStrLn stderr ("Body:" :: BS.ByteString)
                      hPutStrLn stderr body
                      pure (pure (), Nothing)
               -- There has been changes, don't download but upload
              False -> do
                note ("There has been changes so uploading them")
                pure (doUpload actualFileContents, Nothing)
          in do
            atomicWriteIORef listenToEvents False
            atomicModifyFile Nothing lockFilePath filePath f
            atomicWriteIORef listenToEvents True
    let
      eventFunction event = do
        listen <- readIORef listenToEvents
        case listen of
          True -> case event of
            Added{} -> note "File monitor has caught file added so uploading" >> uploadFile
            Modified{} -> note "File monitor has caught file modified so uploading" >> uploadFile
            Removed{} -> pure ()
          False -> pure ()
    note ("Initial upload")
    uploadFile
    System.FSNotify.withManager $ \mgr -> do
      _ <- watchFile mgr filePath eventFunction
      forever $ do
        threadDelay serverCheckMicroseconds
        downloadFile

watchFile :: WatchManager -> FilePath -> (Event -> IO ()) -> IO StopListening
watchFile mgr filePath eventFunction = go where
  go =
    let
      (directory, _) = splitFileName filePath
      isValidEvent event = eventPath event == filePath
    in
      watchDir mgr directory isValidEvent eventFunction

note :: BS.ByteString -> IO ()
note = hPutStrLn stderr
