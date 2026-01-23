module Oasis.Tui.Render.Markdown
  ( renderMarkdown
  ) where

import Relude
import Brick.AttrMap (attrName)
import Brick.Types (Widget(..), vpSize, getContext, availWidthL, Size(..))
import Brick.Widgets.Core
import Brick.Widgets.Skylighting (highlightFromMap)
import Skylighting.Syntax (defaultSyntaxMap)
import Data.Char (isSpace, ord)
import qualified Data.Text as T
import qualified Data.List as List
import Lens.Micro ((^.))
import qualified Graphics.Text.Width as W

renderMarkdown :: (Ord n, Show n) => n -> Text -> Widget n
renderMarkdown viewportName input =
  Widget Fixed Fixed $ do
    ctx <- getContext
    mVp <- unsafeLookupViewport viewportName
    let wrapWidth = max 1 $ case mVp of
          Just vp -> let (w, _) = vp ^. vpSize in w
          Nothing -> ctx ^. availWidthL
        blocks = parseMarkdown input
        content = vBox (map (renderBlock wrapWidth) blocks)
    render content

renderBlock :: Int -> Block -> Widget n
renderBlock wrapWidth = \case
  Heading _ title -> withAttr (attrName "mdHeading") (renderWrappedInline wrapWidth title)
  Paragraph ls -> renderWrappedInline wrapWidth (T.intercalate "\n" (map T.strip ls))
  Bullet items -> vBox (map (renderBullet wrapWidth) items)
  CodeBlock lang code ->
    let langName = if T.null (T.strip lang) then "text" else lang
    in highlightFromMap defaultSyntaxMap langName code
  Blank n -> vBox (replicate n (renderLine ""))

renderBullet :: Int -> Text -> Widget n
renderBullet wrapWidth item =
  let innerWidth = max 1 (wrapWidth - 2)
      wrapped = wrapText innerWidth (T.strip item)
  in case wrapped of
      [] -> txt "- "
      (firstLine:rest) ->
        vBox
          ( txt ("- " <> firstLine)
          : map (renderLine . ("  " <>)) rest
          )

renderWrapped :: Int -> Text -> Widget n
renderWrapped wrapWidth t =
  let wrapped = wrapText wrapWidth (T.strip t)
  in case wrapped of
  [] -> renderLine ""
  xs -> vBox (map renderLine xs)

renderWrappedInline :: Int -> Text -> Widget n
renderWrappedInline wrapWidth t =
  let linesIn = T.splitOn "\n" (T.strip t)
      rendered = concatMap (renderWrappedLine wrapWidth) linesIn
  in case rendered of
      [] -> renderLine ""
      xs -> vBox xs

renderWrappedLine :: Int -> Text -> [Widget n]
renderWrappedLine wrapWidth line
  | T.null line = [renderLine ""]
  | otherwise =
      let wrapped = wrapText wrapWidth line
      in if null wrapped
          then [renderLine ""]
          else map renderLine wrapped

renderLine :: Text -> Widget n
renderLine line = if T.null line then txt " " else txt line

wrapText :: Int -> Text -> [Text]
wrapText maxWidth t
  | maxWidth <= 1 = [t]
  | T.null (T.strip t) = []
  | otherwise =
      let segments = T.splitOn "\n" t
      in concatMap (wrapSingleLine maxWidth) segments
  where
    wrapSingleLine limit line
      | T.null line = [""]
      | otherwise = finalize (go [] "" 0 (T.words line))
      where
        finalize [] = []
        finalize xs = xs

        go acc current curW [] =
          if T.null current
            then reverse acc
            else reverse (current : acc)
        go acc current curW (w:ws)
          | T.null current =
              let wWidth = textWidth w
              in if wWidth <= limit
                    then go acc w wWidth ws
                    else
                      let parts = splitWordByWidth limit w
                      in case reverse parts of
                           [] -> go acc "" 0 ws
                           (lastPart:revRest) ->
                             let acc' = revRest <> acc
                                 curW' = textWidth lastPart
                             in go acc' lastPart curW' ws
          | otherwise =
              let wWidth = textWidth w
                  addWidth = curW + 1 + wWidth
              in if addWidth <= limit
                    then go acc (current <> " " <> w) addWidth ws
                    else go (current : acc) "" 0 (w:ws)

    textWidth = W.safeWctwidth

splitWordByWidth :: Int -> Text -> [Text]
splitWordByWidth limit txt
  | limit <= 1 = [txt]
  | T.null txt = []
  | otherwise = step [] "" 0 txt
  where
    step acc cur curW rest =
      case T.uncons rest of
        Nothing ->
          if T.null cur then reverse acc else reverse (cur : acc)
        Just (c, remaining) ->
          let cw = W.safeWcwidth c
              nextW = curW + cw
          in if nextW > limit && not (T.null cur)
                then step (cur : acc) "" 0 rest
                else
                  let cur' = T.snoc cur c
                      curW' = if cw < 0 then curW else nextW
                  in step acc cur' curW' remaining


data Block
  = Heading Int Text
  | Paragraph [Text]
  | Bullet [Text]
  | CodeBlock Text Text
  | Blank Int

parseMarkdown :: Text -> [Block]
parseMarkdown input = go (lines input) []
  where
    go [] acc = reverse acc
    go (ln:rest) acc
      | isBlank ln =
        let (blanks, remaining) = span isBlank (ln:rest)
        in go remaining (Blank (length blanks) : acc)
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

sanitizeEmoji :: Text -> Text
sanitizeEmoji = T.concatMap replaceChar
  where
    placeholder = "□"
    replaceChar c
      | isEmojiChar c = placeholder
      | isEmojiJoiner c = ""
      | otherwise = T.singleton c

isEmojiJoiner :: Char -> Bool
isEmojiJoiner c =
  let cp = ord c
  in cp == 0x200D || inRange 0xFE00 0xFE0F cp

isEmojiChar :: Char -> Bool
isEmojiChar c =
  let cp = ord c
  in or
      [ inRange 0x1F300 0x1F5FF cp
      , inRange 0x1F600 0x1F64F cp
      , inRange 0x1F680 0x1F6FF cp
      , inRange 0x1F900 0x1F9FF cp
      , inRange 0x1FA70 0x1FAFF cp
      , inRange 0x2600 0x26FF cp
      , inRange 0x2700 0x27BF cp
      , inRange 0x1F1E6 0x1F1FF cp
      , inRange 0x1F3FB 0x1F3FF cp
      ]

inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = x >= lo && x <= hi
