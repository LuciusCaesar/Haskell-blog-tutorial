module HsBlog (
  main, 
  process) 
where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
-- main =
--   getArgs >>= \case
--     [] ->
--       getContents >>= \content ->
--         putStrLn (process "Empty title" content)
--     [input, output] ->
--       doesFileExist input >>= \exist ->
--         if exist
--           then
--             readFile input >>= \content ->
--               whenIO confirm $ writeFile output $ process input content
--           else putStrLn "file does not exist"
--     _ -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- getContents
      putStrLn (process "Empty title" content)
    [input, output] -> do
      exist <- doesFileExist input
      if exist
        then do
          content <- readFile input
          whenIO confirm $ writeFile output $ process input content
        else putStrLn "file does not exist"
    _ -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"


process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

whenIO :: IO Bool -> IO () -> IO ()
-- whenIO cond action =
--   cond >>= \result ->
--     if result
--       then action
--       else pure ()
whenIO cond action = do
  result <- cond
  if result then action else pure ()

confirm :: IO Bool
-- confirm =
--   putStrLn "Are you sure? (y/n)"
--     *> getLine
--     >>= \case
--       "y" -> pure True
--       "n" -> pure False
--       _ ->
--         putStrLn "Invalid response. use y or n"
--           *> confirm
confirm = do
  putStrLn "Are you sure? (y/n)"
  line <- getLine
  case line of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response, use y or no"
      confirm
