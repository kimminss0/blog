module Site.Context
  ( postCtx,
    dropIndexHtml,
  )
where

import Control.Monad (msum)
import qualified Data.Time as DT
import Data.Time.Clock
import Data.Time.Format
import Hakyll
import System.FilePath (splitFileName, takeDirectory)

postCtx :: Context String
postCtx =
  dateFieldWithZone zone "date" "%F"
    <> dateFieldWithZone zone "datetime" "%Y-%m-%dT%H:%M:%S%Ez"
    <> updateFieldWithZone zone "lastmodDate" "%F"
    <> updateFieldWithZone zone "lastmodDatetime" "%Y-%m-%dT%H:%M:%S%Ez"
    <> dropIndexHtml "url"
    <> defaultContext
  where
    zone = DT.TimeZone (9 * 60) False "KST"

dropIndexHtml :: String -> Context a
dropIndexHtml key = mapContext transform (urlField key)
  where
    transform url = case splitFileName url of
      (p, "index.html") -> takeDirectory p
      _ -> url

dateFieldWithZone :: DT.TimeZone -> String -> String -> Context String
dateFieldWithZone zone key format = field key $ \i -> do
  let locale = defaultTimeLocale
  time <- getItemZonedTime "published" zone locale $ itemIdentifier i
  return $ formatTime locale format time

updateFieldWithZone :: DT.TimeZone -> String -> String -> Context String
updateFieldWithZone zone key format = field key $ \i -> do
  let locale = defaultTimeLocale
  time <- getItemZonedTime "updated" zone locale $ itemIdentifier i
  return $ formatTime locale format time

getItemZonedTime ::
  (MonadMetadata m, MonadFail m) =>
  String ->
  DT.TimeZone ->
  TimeLocale ->
  Identifier ->
  m DT.ZonedTime
getItemZonedTime key zone locale id' = do
  metadata <- getMetadata id'
  let tryField k fmt =
        lookupString k metadata
          >>= parseTime' fmt
          >>= Just . DT.utcToZonedTime zone
  maybe empty' return $ msum [tryField key fmt | fmt <- formats]
  where
    empty' = fail $ "getItemZonedTime: " ++ "could not parse time for " ++ show id'
    parseTime' :: String -> String -> Maybe UTCTime
    parseTime' = parseTimeM True locale
    formats =
      [ "%Y-%m-%d",
        "%Y-%m-%dT%H:%M:%S%EZ",
        "%Y-%m-%dT%H:%M:%S"
      ]
