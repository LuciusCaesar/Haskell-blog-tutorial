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

import Numeric.Natural

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  -- currentParagraph is an accumulator
  let paragraph = Paragraph (unlines (reverse currentParagraph))
   in -- will create a Paragraph, when needed only, by and concatanating all accumulated lines.
      case txts of
        [] -> [paragraph] -- if there is no text left to parse, just return the paragraph.
        currentLine : rest ->
          if trim currentLine == "" -- Empty lines separate new paragrphs
            then paragraph : parseLines [] rest
            else parseLines (currentLine : currentParagraph) rest -- otherwise, we accumulate and keep going

trim :: String -> String
trim = unwords . words