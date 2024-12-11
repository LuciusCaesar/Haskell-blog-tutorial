module Markup
  ( Document,
    Structure
      ( Heading,
        Paragraph,
        UnorderedList,
        OrderedList,
        CodeBlock
      ),
  )
where

import GHC.Data.Maybe (maybeToList)
import Numeric.Natural

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show, Eq)

parse :: String -> Document
parse = parseLines Nothing . lines

-- parseLines :: [String] -> [String] -> Document
-- parseLines currentParagraph txts =
--   -- currentParagraph is an accumulator
--   let paragraph = Paragraph (unlines (reverse currentParagraph))
--    in -- will create a Paragraph, when needed only, by and concatanating all accumulated lines.
--       case txts of
--         [] -> [paragraph] -- if there is no text left to parse, just return the paragraph.
--         currentLine : rest ->
--           if trim currentLine == "" -- Empty lines separate new paragrphs
--             then paragraph : parseLines [] rest
--             else parseLines (currentLine : currentParagraph) rest -- otherwise, we accumulate and keep going

parseLines :: Maybe Structure -> [String] -> Document
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

trim :: String -> String
trim = unwords . words
