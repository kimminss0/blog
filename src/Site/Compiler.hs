{-# LANGUAGE OverloadedStrings #-}

module Site.Compiler
  ( renderPandocCustom,
    pandocCustomCompiler,
    renderLualatex,
    lualatexCompiler,
  )
where

import Hakyll
import System.FilePath (replaceExtension, takeDirectory, takeFileName)
import System.IO.Silently (silence)
import System.Process (CreateProcess (cwd), createProcess, proc, waitForProcess)
import Text.Pandoc.Options

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

renderLualatex :: Item String -> Compiler (Item TmpFile)
renderLualatex item = do
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

lualatexCompiler :: Compiler (Item TmpFile)
lualatexCompiler = getResourceString >>= renderLualatex
