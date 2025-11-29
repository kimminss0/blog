{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Site.Compiler
import Site.Context
import Site.Route

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
    route $ latexToImage `composeRoutes` setExtension "png"
    compile lualatexCompiler

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
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/with-comments.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <-
        fmap (take maxFeedPosts) . recentFirst =<< loadAllSnapshots "posts/*" "content"
      renderAtom feedConfig feedCtx posts

  create ["posts.html"] $ do
    route $ idRoute `composeRoutes` appendIndex
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
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
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
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
      posts <-
        fmap (take maxIndexPosts) . recentFirst =<< loadAllSnapshots "posts/*" "content"
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

feedConfig :: FeedConfiguration
feedConfig =
  FeedConfiguration
    { feedTitle = "blog.mskim.org",
      feedRoot = "https://blog.mskim.org",
      feedDescription = "Minseo Kim's Blog",
      feedAuthorName = "Minseo Kim",
      feedAuthorEmail = "kimminss0@outlook.kr"
    }

maxIndexPosts :: Int
maxIndexPosts = 20

maxFeedPosts :: Int
maxFeedPosts = 10
