-- HsBlog.hs
module HsBlog
  ( convertSingle,
    convertDirectory,
    process,
  )
where

import HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import qualified HsBlog.Directory as Directory
import HsBlog.Env (Env, defaultEnv)
import System.IO

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: Env -> FilePath -> FilePath -> IO ()
convertDirectory = Directory.convertDirectory

process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse
