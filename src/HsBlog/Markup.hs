-- | This module provides a simple markup parser for the HsBlog project.
module HsBlog.Markup
  ( -- * Types
    Document,
    Structure
      ( Heading,
        Paragraph,
        UnorderedList,
        OrderedList,
        CodeBlock
      ),
    -- * Functions
    parse,
  )
where

import Data.Maybe (maybeToList)
import Numeric.Natural

-- | A Markup document is a list of structures.
type Document = [Structure]

-- | A Markup structure is a heading, paragraph, list or code block.
data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show, Eq)

-- | Parse a string into a Markup document.
parse :: String -> Document
parse = parseLines Nothing . lines

-- | Parse a list of lines into a Markup document.
parseLines 
  :: Maybe Structure -- ^ Current context: the already parsed structure to which adding the next line
  -> [String] -- ^ Remaining lines to parse
  -> Document -- ^ Parsed document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context
    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    -- CodeBlock
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [trim line])) rest)
    -- OrderedList case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
    -- Paragraph case
    currentLine : rest ->
      let line = trim currentLine
       in if line == ""
            then maybe id (:) context (parseLines Nothing rest)
            else case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

-- | Trim leading and trailing whitespace.
trim :: String -> String
trim = unwords . words
