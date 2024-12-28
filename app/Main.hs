{-# LANGUAGE LambdaCase #-}

-- | Entry point for the hs-blog-gen program
module Main where

import OptParse
import System.IO
import Control.Exception (bracket)
import HsBlog

-- main :: IO ()
-- main = do
--   options <- parse
--   case options of
--     ConvertDir input output ->
--       HsBlog.convertDirectory input output
--     ConvertSingle input output -> do
--       (title, inputHandle) <-
--         case input of
--           Stdin ->
--             pure ("", stdin)
--           InputFile file ->
--             (,) file <$> openFile file ReadMode

--       outputHandle <-
--         case output of
--           Stdout -> pure stdout
--           OutputFile file -> do
--             exists <- doesFileExist file
--             shouldOpenFile <-
--               if exists
--                 then confirm
--                 else pure True
--             if shouldOpenFile
--               then openFile file WriteMode
--               else exitFailure

--       HsBlog.convertSingle title inputHandle outputHandle
--       hClose inputHandle
--       hClose outputHandle
main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.convertDirectory input output
    ConvertSingle input output ->
      let
        withInputHandle :: (String -> Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin ->
              action "" stdin
            InputFile file ->
              bracket
                (openFile file ReadMode)
                hClose
                (action file)
        withOutputHandle :: (Handle -> IO a) -> IO a
        withOutputHandle action =
          case output of
            Stdout -> action stdout
            OutputFile file ->
              bracket
                (openFile file WriteMode)
                hClose
                action
      in withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)

------------------------------------------------

-- * Utilities

-- | Confirm user action
confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ ->
        putStrLn "Invalid response. use y or n"
          *> confirm
