{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (msum)
import qualified Data.Time as DT
import Data.Time.Clock
import Data.Time.Format
import Hakyll
import System.FilePath
import System.IO.Silently (silence)
import System.Process (CreateProcess (cwd), createProcess, proc, waitForProcess)
import Text.Pandoc.Options

main :: IO ()
main = hakyllWith config $ do
  match "static/**" $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

  match ("assets/**" .&&. complement "assets/**.css") $ do
    route idRoute
    compile copyFileCompiler

  match "assets/**.css" $ do
    route idRoute
    compile compressCssCompiler

  match "latex-src/**.tex" $ do
    route $ latexRoute `composeRoutes` setExtension "png"
    compile $ getResourceString >>= lualatex

  match "about.md" $ do
    route $ setExtension "html" `composeRoutes` appendIndex
    compile $
      pandocCustomCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "posts/*" $ do
    route $ setExtension "html" `composeRoutes` appendIndex `composeRoutes` slugToPath
    compile $
      getResourceString
        >>= renderPandocCustom
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx

  create ["posts.html"] $ do
    route $ idRoute `composeRoutes` appendIndex
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Posts"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let sitemapCtx =
            listField "posts" postCtx (return posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  match "404.md" $ do
    route $ setExtension "html"
    compile $
      pandocCustomCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx

  match "templates/*" $ compile templateBodyCompiler

config :: Configuration
config =
  defaultConfiguration
    { deployCommand = "./deploy.sh"
    }

appendIndex :: Routes
appendIndex =
  customRoute $
    (\(p, e) -> p </> "index" <.> e) . splitExtension . toFilePath

dropIndexHtml :: String -> Context a
dropIndexHtml key = mapContext transform (urlField key)
  where
    transform url = case splitFileName url of
      (p, "index.html") -> takeDirectory p
      _ -> url

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

renderPandocCustom :: Item String -> Compiler (Item String)
renderPandocCustom =
  let readerOpts = defaultHakyllReaderOptions
      writerOpts =
        defaultHakyllWriterOptions
          { writerHTMLMathMethod = MathJax ""
          }
   in renderPandocWith readerOpts writerOpts

pandocCustomCompiler :: Compiler (Item String)
pandocCustomCompiler = getResourceBody >>= renderPandocCustom

slugToPath :: Routes
slugToPath =
  gsubRoute "/[0-9]*-" $
    replaceAll "-" (const "/")
      . replaceAll "/0*" (const "/")

latexRoute :: Routes
latexRoute = gsubRoute "latex-src" $ const "assets/images"

lualatex :: Item String -> Compiler (Item TmpFile)
lualatex item = do
  TmpFile texPath <- newTmpFile "lualatex/main.tex"
  let tmpDir = takeDirectory texPath
      texFile = takeFileName texPath
      pngPath = replaceExtension texPath "png"

  unsafeCompiler $ do
    writeFile texPath (itemBody item)
    (_, _, _, handle) <-
      silence $
        createProcess
          (proc "lualatex" ["-halt-on-error", "-shell-escape", texFile]) {cwd = Just tmpDir}
    _ <- waitForProcess handle
    return ()

  makeItem $ TmpFile pngPath
