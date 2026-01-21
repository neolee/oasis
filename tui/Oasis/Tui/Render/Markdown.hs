module Oasis.Tui.Render.Markdown
  ( renderMarkdown
  ) where

import Relude
import Brick.AttrMap (attrName)
import Brick.Types (Widget)
import Brick.Widgets.Core
import Brick.Widgets.Skylighting (highlightFromMap)
import Skylighting.Syntax (defaultSyntaxMap)
import Data.Char (isSpace)
import qualified Data.Text as T

renderMarkdown :: Text -> Widget n
renderMarkdown input =
  vBox (intersperse (padTop (Pad 1) (txt "")) (map renderBlock (parseMarkdown input)))

renderBlock :: Block -> Widget n
renderBlock = \case
  Heading _ title -> withAttr (attrName "mdHeading") (txtWrap title)
  Paragraph ls -> txtWrap (unlines ls)
  Bullet items -> vBox (map renderBullet items)
  CodeBlock lang code ->
    let langName = if T.null (T.strip lang) then "text" else lang
    in highlightFromMap defaultSyntaxMap langName code

renderBullet :: Text -> Widget n
renderBullet item = txtWrap ("- " <> item)

data Block
  = Heading Int Text
  | Paragraph [Text]
  | Bullet [Text]
  | CodeBlock Text Text

parseMarkdown :: Text -> [Block]
parseMarkdown input = go (lines input) []
  where
    go [] acc = reverse acc
    go (ln:rest) acc
      | isBlank ln = go rest acc
      | isFenceStart ln =
          let (lang, afterStart) = parseFenceStart ln rest
              (codeLines, remaining) = break isFenceEnd afterStart
              remaining' = drop 1 remaining
          in go remaining' (CodeBlock lang (unlines codeLines) : acc)
      | isHeading ln =
          let (level, title) = parseHeading ln
          in go rest (Heading level title : acc)
      | isBullet ln =
          let (bullets, remaining) = span isBullet (ln:rest)
          in go remaining (Bullet (map dropBullet bullets) : acc)
      | otherwise =
          let (paraLines, remaining) = span isParagraphLine (ln:rest)
          in go remaining (Paragraph paraLines : acc)

    isParagraphLine t =
      not (isBlank t || isFenceStart t || isHeading t || isBullet t)

isBlank :: Text -> Bool
isBlank = all isSpace . toString . T.strip

isFenceStart :: Text -> Bool
isFenceStart t = "```" `T.isPrefixOf` T.stripStart t

isFenceEnd :: Text -> Bool
isFenceEnd t = "```" `T.isPrefixOf` T.stripStart t

parseFenceStart :: Text -> [Text] -> (Text, [Text])
parseFenceStart firstLine rest =
  let lang = T.strip (T.drop 3 (T.stripStart firstLine))
  in (lang, rest)

isHeading :: Text -> Bool
isHeading t =
  let trimmed = T.stripStart t
  in "#" `T.isPrefixOf` trimmed

parseHeading :: Text -> (Int, Text)
parseHeading t =
  let trimmed = T.stripStart t
      hashes = takeWhile (== '#') (toString trimmed)
      level = min 3 (length hashes)
      title = T.strip (T.drop level trimmed)
  in (level, title)

isBullet :: Text -> Bool
isBullet t =
  let trimmed = T.stripStart t
  in ("- " `T.isPrefixOf` trimmed) || ("* " `T.isPrefixOf` trimmed)

dropBullet :: Text -> Text
dropBullet t =
  let trimmed = T.stripStart t
  in fromMaybe trimmed (T.stripPrefix "- " trimmed <|> T.stripPrefix "* " trimmed)
