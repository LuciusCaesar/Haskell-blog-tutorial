{-# LANGUAGE LambdaCase #-}

-- | Entry point for the hs-blog-gen program
module Main where

import OptParse
import System.IO
import HsBlog

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output env ->
      HsBlog.convertDirectory env input output
    ConvertSingle input output ->
      let
        withInputHandle :: (String -> Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin ->
              action "" stdin
            InputFile file ->
              withFile file ReadMode
                (action file)
        withOutputHandle :: (Handle -> IO a) -> IO a
        withOutputHandle action =
          case output of
            Stdout -> action stdout
            OutputFile file ->
              withFile file WriteMode
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
