{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Network.HTTP.SyncFile (syncFile)
import Network.URI (URI(URI, uriAuthority, uriPath), parseAbsoluteURI, URIAuth(URIAuth, uriRegName, uriPort))

import Options.Applicative (
  Parser, ParserInfo,
  option, long, help, value, showDefault, auto, str,
  info, helper, fullDesc,
  execParser)
import Data.Semigroup ((<>))

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import qualified Data.ByteString.Char8 as BSC

import Safe (tailMay, readMay)

data Options = Options { urlString :: String, filename :: FilePath, period :: Int }

optionParser :: Parser Options
optionParser = Options
  <$> option str  ( long "url" <> help "URL of remove file: e.g. http://example.com:8080/remote.txt")
  <*> option str  ( long "filename" <> help "Local file name")
  <*> option auto ( long "period" <> help "Seconds between checking server" <> value 10 <> showDefault)

optionParserInfo :: ParserInfo Options
optionParserInfo = info (helper <*> optionParser) fullDesc

main :: IO ()
main = do
  Options{urlString, filename, period} <- execParser optionParserInfo
  -- Parsing the url. Should probably be a function itself in a library somewhere.
  case (parseAbsoluteURI urlString) of
    Just URI{uriAuthority, uriPath} -> case uriAuthority of
      Just URIAuth{uriRegName, uriPort} -> do
        port <- case tailMay uriPort of
          Just portStr -> case readMay portStr of
            Just port -> pure port
            Nothing -> hPutStrLn stderr "Bad port string" >> exitFailure
          Nothing -> pure 80
        syncFile (BSC.pack uriRegName) port (BSC.pack uriPath) (period * 1000000) filename
      Nothing -> hPutStrLn stderr "Require absolute url" >> exitFailure
    Nothing -> hPutStrLn stderr "Bad url argument" >> exitFailure
