{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Header.Sanitize
-- Description : Logic for sanitizing license headers
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains functions related to sanitizing license headers. Because
-- license headers are just regular comments in given programming language, they
-- need to have correct syntax in order to avoid causing compile/runtime errors.
-- Because header manipulation done by /Headroom/ can disrupt the comment syntax
-- structure, sanitizing the header is the last step done in the flow, making
-- sure that license header syntax is not broken.
module Headroom.Header.Sanitize
    ( findPrefix
    , sanitizeSyntax
    , stripCommentSyntax
    )
where

import Headroom.Config.Types (HeaderSyntax (..))
import qualified Headroom.Data.Regex as R
import qualified Headroom.Data.Text as T
import RIO
import qualified RIO.Text as T

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Tries to find comment prefix in given comment. By /prefix/ it's meant
-- either the line prefix used for block comment syntax (like @*@ at start of
-- each line between opening and closing pattern - @/* */@) or line comment
-- syntax (just the syntax for comment itself - like @//@ or @--@). If such
-- prefix is found, it's then added to the input 'HeaderSyntax'.
--
-- >>> import Headroom.Data.Regex (re)
-- >>> findPrefix (BlockComment [re|^\/\*|] [re|\*\/$|] Nothing) "/*\n * foo\n * bar\n */"
-- BlockComment "^\\/\\*" "\\*\\/$" (Just " *")
findPrefix
    :: HeaderSyntax
    -- ^ describes comment syntax of the header
    -> Text
    -- ^ text containint the comment
    -> HeaderSyntax
    -- ^ input 'HeaderSyntax' with added prefix (if found)
findPrefix syntax text = case syntax of
    BlockComment s e _ -> BlockComment s e commonPrefix
    LineComment s _ -> LineComment s (linePrefix s)
  where
    filtered = filter cond . T.toLines $ text
    cond = \t -> (not . T.null . T.strip $ t) && isCommentBody syntax t
    commonPrefix = fmap T.stripEnd (T.commonLinesPrefix . T.fromLines $ filtered)
    linePrefix start = listToMaybe . mapMaybe (matchPrefix start) $ filtered
    matchPrefix start line = do
        (candidate, _) <- listToMaybe $ R.scan start line
        guard (not $ T.null candidate)
        guard $ candidate `T.isPrefixOf` line
        pure candidate

-- | Sanitizes given header text to make sure that each comment line starts with
-- appropriate prefix (if defined within given 'HeaderSyntax'). For block
-- comments, this is to make it visually unified, but for line comments it's
-- necessary in order not to break syntax of target source code file.
--
-- >>> import Headroom.Data.Regex (re)
-- >>> sanitizeSyntax (LineComment [re|^--|] (Just "--")) "-- foo\nbar"
-- "-- foo\n-- bar"
sanitizeSyntax
    :: HeaderSyntax
    -- ^ header syntax definition that may contain prefix
    -> Text
    -- ^ header to sanitize
    -> Text
    -- ^ sanitized header
sanitizeSyntax syntax = mapCommentLines syntax (process mPrefix)
  where
    process Nothing l = Just l
    process (Just p) l
        | p `T.isPrefixOf` l = Just l
        | otherwise = Just $ addPrefix p l
    mPrefix = case syntax of
        BlockComment _ _ p -> p
        LineComment _ p -> p

-- | Strips comment syntax from given text.
--
-- >>> import Headroom.Data.Regex (re)
-- >>> stripCommentSyntax (LineComment [re|^--|] (Just "--")) "-- a\n-- b"
-- " a\n b"
stripCommentSyntax
    :: HeaderSyntax
    -- ^ copyright header syntax
    -> Text
    -- ^ input text from which to strip the syntax
    -> Text
    -- ^ processed text
stripCommentSyntax syntax = T.fromLines . go [] . T.toLines . T.strip
  where
    (s, e, p) = case syntax of
        BlockComment s' e' p' -> (Just s', Just e', p')
        LineComment s' p' -> (Just s', Nothing, p')
    nil = const . const $ ""
    rep = \pt l -> maybe l (\pt' -> R.replaceFirst pt' nil l) pt
    dp = \pt l -> maybe l (\pt' -> T.replaceFirst pt' "" l) pt
    go agg [] = reverse agg
    go [] (x : xs) = go [rep s . rep e . dp p $ x] xs
    go agg [x] = go ((rep e . dp p $ x) : agg) []
    go agg (x : xs) = go (dp p x : agg) xs

------------------------------  PRIVATE FUNCTIONS  -----------------------------

addPrefix :: Text -> Text -> Text
addPrefix p l
    | " " `T.isSuffixOf` p || " " `T.isPrefixOf` l = p <> l
    | T.null l = p
    | otherwise = p <> " " <> l

mapCommentLines
    :: (Foldable t)
    => HeaderSyntax
    -> (Text -> t Text)
    -> Text
    -> Text
mapCommentLines syntax f = T.mapLinesF $ \case
    line
        | isCommentBody syntax line -> toList . f $ line
        | otherwise -> [line]

isCommentBody :: HeaderSyntax -> Text -> Bool
isCommentBody (LineComment _ _) _ = True
isCommentBody (BlockComment s e _) l = not $ R.isMatch s l || R.isMatch e l
