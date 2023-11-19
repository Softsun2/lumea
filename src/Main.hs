module Main where

import qualified Text.Pandoc as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  result <- P.runIO $ do
    doc <- P.readMarkdown P.def (T.pack "[testing](url)")
    P.writeOrg P.def doc
  rst <- P.handleError result
  TIO.putStrLn rst
