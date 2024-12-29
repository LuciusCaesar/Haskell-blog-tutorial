-- HsBlog.hs
-- | Main module for the HsBlog application.
module HsBlog
  ( convertSingle, -- ^ Convert a single file
    convertDirectory, -- ^ Convert a directory
    process, -- ^ Convert a string
  )
where

import HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import qualified HsBlog.Directory as Directory
import HsBlog.Env (Env, defaultEnv)
import System.IO

-- | Convert a single file from input to output.
convertSingle 
  :: String -- ^ Title of the page
  -> Handle -- ^ Input handle
  -> Handle -- ^ Output handle
  -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

-- | Convert a directory from input to output.
convertDirectory 
  :: Env -- ^ Environment containing blog name and stylesheet path
  -> FilePath -- ^ Input directory
  -> FilePath -- ^ Output directory
  -> IO ()
convertDirectory = Directory.convertDirectory

-- | Convert a string to HTML.
process 
  :: String -- ^ Title of the page
  -> String -- ^ Content to convert in Markup syntax
  -> String -- ^ HTML representation of the content
process title = Html.render . convert defaultEnv title . Markup.parse
