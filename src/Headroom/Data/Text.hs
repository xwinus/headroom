{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Data.Text
-- Description : Additional utilities for text manipulation
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module containing bunch of useful functions for working with text.
module Headroom.Data.Text
    ( read
    , commonLinesPrefix
    , replaceFirst

      -- * Working with text lines
    , mapLines
    , mapLinesF
    , fromLines
    , toLines
    )
where

import RIO
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Similar to 'T.commonPrefixes', but tries to find common prefix for all
-- lines in given text.
--
-- >>> commonLinesPrefix "-- first\n-- second\n-- third"
-- Just "-- "
commonLinesPrefix
    :: Text
    -- ^ lines of text to find prefix for
    -> Maybe Text
    -- ^ found longest common prefixs
commonLinesPrefix text = go (toLines text) Nothing
  where
    go [] acc = acc
    go (x : xs) Nothing = go xs (Just x)
    go (x : xs) (Just acc) = case T.commonPrefixes x acc of
        Just (n, _, _) -> go xs (Just n)
        _ -> Nothing

-- | Similar to 'T.replace', but replaces only very first occurence of pattern.
--
-- >>> replaceFirst ":" "/" "a : b : c"
-- "a / b : c"
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst ptrn substitution text
    | T.null ptrn || T.null back = text
    | otherwise = processed
  where
    (front, back) = TP.breakOn ptrn text
    processed = mconcat [front, substitution, T.drop (T.length ptrn) back]

-- | Maps given function over individual lines of the given text.
--
-- >>> mapLines ("T: " <>) "foo zz\nbar"
-- "T: foo zz\nT: bar"
mapLines
    :: (Text -> Text)
    -- ^ function to map over individual lines
    -> Text
    -- ^ input text
    -> Text
    -- ^ resulting text
mapLines fn = mapLinesF (Just <$> fn)

-- | Similar to 'mapLines', but the mapping function returns 'Foldable', which
-- gives some more control over outcome. After mapping over all individual
-- lines, results are folded and concatenated, which allows for example
-- filtering out some lines.
--
-- >>> mapLinesF (\l -> if l == "bar" then Nothing else Just l) "foo\nbar"
-- "foo"
mapLinesF
    :: (Foldable t)
    => (Text -> t Text)
    -- ^ function to map over inividual lines
    -> Text
    -- ^ input text
    -> Text
    -- ^ resulting text
mapLinesF f = fromLines . concatMap toList . go . toLines
  where
    go [] = []
    go (x : xs) = f x : go xs

-- | Same as 'readMaybe', but takes 'Text' as input instead of 'String'.
--
-- >>> read "123" :: Maybe Int
-- Just 123
read
    :: (Read a)
    => Text
    -- ^ input text to parse
    -> Maybe a
    -- ^ parsed value
read = readMaybe . T.unpack

-- | Similar to 'T.unlines', but does not automatically adds @\n@ at the end
-- of the text. Advantage is that when used together with 'toLines', it doesn't
-- ocassionaly change the newlines ad the end of input text:
--
-- >>> fromLines . toLines $ "foo\nbar"
-- "foo\nbar"
--
-- >>> fromLines . toLines $ "foo\nbar\n"
-- "foo\nbar\n"
--
-- Other examples:
--
-- >>> fromLines []
-- ""
--
-- >>> fromLines ["foo"]
-- "foo"
--
-- >>> fromLines ["first", "second"]
-- "first\nsecond"
--
-- >>> fromLines ["first", "second", ""]
-- "first\nsecond\n"
fromLines
    :: [Text]
    -- ^ lines to join
    -> Text
    -- ^ text joined from individual lines
fromLines = T.intercalate "\n"

-- | Similar to 'T.lines', but does not drop trailing newlines from output.
-- Advantage is that when used together with 'fromLines', it doesn't ocassionaly
-- change the newlines ad the end of input text:
--
-- >>> fromLines . toLines $ "foo\nbar"
-- "foo\nbar"
--
-- >>> fromLines . toLines $ "foo\nbar\n"
-- "foo\nbar\n"
--
-- Other examples:
--
-- >>> toLines ""
-- []
--
-- >>> toLines "first\nsecond"
-- ["first","second"]
--
-- >>> toLines "first\nsecond\n"
-- ["first","second",""]
toLines
    :: Text
    -- ^ text to break into lines
    -> [Text]
    -- ^ lines of input text
toLines input
    | T.null input = []
    | otherwise = T.split (== '\n') input
