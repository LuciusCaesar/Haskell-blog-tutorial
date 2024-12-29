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
import HsBlog.Env (Env)
import System.IO

convertSingle :: Env -> Html.Title -> Handle -> Handle -> IO ()
convertSingle env title input output = do
  content <- hGetContents input
  hPutStrLn output (process env title content)

convertDirectory :: Env -> FilePath -> FilePath -> IO ()
convertDirectory = Directory.convertDirectory

process :: Env -> Html.Title -> String -> String
process env title = Html.render . convert env title . Markup.parse
