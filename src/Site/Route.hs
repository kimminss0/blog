module Site.Route
  ( appendIndex,
    slugToPath,
    latexToImage,
  )
where

import Hakyll
import System.FilePath ( (</>), (<.>), splitExtension )

appendIndex :: Routes
appendIndex =
  customRoute $
    (\(p, e) -> p System.FilePath.</> "index" System.FilePath.<.> e) . System.FilePath.splitExtension . toFilePath

slugToPath :: Routes
slugToPath =
  gsubRoute "/[0-9]*-" $
    replaceAll "-" (const "/")
      . replaceAll "/0*" (const "/")

latexToImage :: Routes
latexToImage = gsubRoute "latex-src" $ const "assets/images"
