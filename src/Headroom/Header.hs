{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Header
-- Description : Operations with copyright/license headers
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module is the heart of /Headroom/ as it contains functions for working with
-- the /license headers/ and the /source code files/.
module Headroom.Header
    ( -- * Header Info Extraction
      extractHeaderInfo
    , extractHeaderTemplate

      -- * License header manipulation
    , addHeader
    , dropHeader
    , replaceHeader

      -- * Copyright Header Detection
    , findHeader
    , findBlockHeader
    , findLineHeader
    , splitSource
    )
where

import Headroom.Config.Types
    ( CtHeaderConfig
    , CtHeadersConfig
    , HeaderConfig (..)
    , HeaderSyntax (..)
    )
import Headroom.Data.Coerce
    ( coerce
    , inner
    )
import Headroom.Data.Lens (suffixLensesFor)
import Headroom.Data.Regex
    ( Regex
    , isMatch
    )
import Headroom.FileSupport (fileSupport)
import Headroom.FileSupport.Types (FileSupport (..))
import Headroom.FileType (configByFileType)
import Headroom.FileType.Types (FileType)
import Headroom.Header.Sanitize (findPrefix)
import Headroom.Header.Types
    ( HeaderInfo (..)
    , HeaderTemplate (..)
    )
import Headroom.Meta (TemplateType)
import Headroom.SourceCode
    ( CodeLine
    , LineType (..)
    , SourceCode (..)
    , firstMatching
    , fromText
    , lastMatching
    , stripEnd
    , stripStart
    )
import Headroom.Template (Template (..))
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T

suffixLensesFor ["hcHeaderSyntax"] ''HeaderConfig
suffixLensesFor ["hiHeaderPos"] ''HeaderInfo

-- | Extracts info about the processed file to be later used by the header
-- detection/manipulation functions.
extractHeaderInfo
    :: HeaderTemplate
    -- ^ template info
    -> SourceCode
    -- ^ text used for detection
    -> HeaderInfo
    -- ^ resulting file info
extractHeaderInfo ht@HeaderTemplate{..} source =
    let hiFileType = htFileType
        hiHeaderConfig = htConfig
        hiHeaderPos = findHeader hiHeaderConfig source
        hiVariables = fsExtractVariables ht hiHeaderPos source
     in HeaderInfo{..}
  where
    FileSupport{..} = fileSupport htFileType

-- | Constructs new 'HeaderTemplate' from provided data.
extractHeaderTemplate
    :: CtHeadersConfig
    -- ^ configuration for license headers
    -> FileType
    -- ^ type of source code files this template is for
    -> TemplateType
    -- ^ parsed template
    -> HeaderTemplate
    -- ^ resulting template info
extractHeaderTemplate configs fileType template =
    let htConfig = withP (configByFileType configs fileType)
        htTemplateData = fsExtractTemplateData template (hcHeaderSyntax htConfig)
        htFileType = fileType
        htTemplate = template
     in HeaderTemplate{..}
  where
    FileSupport{..} = fileSupport fileType
    withP = \config -> config & hcHeaderSyntaxL %~ headerSyntax
    headerSyntax = \hs -> findPrefix hs (rawTemplate template)

-- | Adds given header at position specified by the 'HeaderInfo'. Does nothing
-- if any header is already present, use 'replaceHeader' if you need to
-- override it.
addHeader
    :: HeaderInfo
    -- ^ additional info about the header
    -> Text
    -- ^ text of the new header
    -> SourceCode
    -- ^ source code where to add the header
    -> SourceCode
    -- ^ resulting source code with added header
addHeader HeaderInfo{..} _ source | isJust hiHeaderPos = source
addHeader HeaderInfo{..} header source = mconcat chunks
  where
    HeaderConfig{..} = hiHeaderConfig
    (before, middle, after) = splitSource hcPutAfter hcPutBefore source
    header' = fromText [] (const $ pure Comment) header
    before' = stripEnd before
    middle' = stripStart middle
    margin (SourceCode ls) mInner mOuter
        | L.null ls = coerce $ replicate mOuter (Code, T.empty)
        | otherwise = coerce $ replicate mInner (Code, T.empty)
    marginT = margin before' hcMarginTopCode hcMarginTopFile
    marginB = margin (middle' <> after) hcMarginBottomCode hcMarginBottomFile
    chunks = [before', marginT, header', marginB, middle', after]

-- | Drops header at position specified by the 'HeaderInfo' from the given
-- source code. Does nothing if no header is present.
dropHeader
    :: HeaderInfo
    -- ^ additional info about the header
    -> SourceCode
    -- ^ text of the file from which to drop the header
    -> SourceCode
    -- ^ resulting text with dropped header
dropHeader (HeaderInfo _ _ Nothing _) source = source
dropHeader (HeaderInfo _ _ (Just (start, end)) _) source = result
  where
    before = inner @_ @[CodeLine] (take start) source
    after = inner @_ @[CodeLine] (drop $ end + 1) source
    result = stripEnd before <> stripStart after

-- | Replaces existing header at position specified by the 'HeaderInfo' in the
-- given text. Basically combines 'addHeader' with 'dropHeader'. If no header
-- is present, then the given one is added to the text.
replaceHeader
    :: HeaderInfo
    -- ^ additional info about the header
    -> Text
    -- ^ text of the new header
    -> SourceCode
    -- ^ text of the file where to replace the header
    -> SourceCode
    -- ^ resulting text with replaced header
replaceHeader fileInfo header = addHeader' . dropHeader'
  where
    addHeader' = addHeader infoWithoutPos header
    dropHeader' = dropHeader fileInfo
    infoWithoutPos = fileInfo & hiHeaderPosL .~ Nothing

-- | Finds header position in given text, where position is represented by
-- line number of first and last line of the header (numbered from zero).
-- Based on the 'HeaderSyntax' specified in given 'HeaderConfig', this function
-- delegates its work to either 'findBlockHeader' or 'findLineHeader'.
--
-- >>> import Headroom.Data.Regex (re)
-- >>> let hc = HeaderConfig ["hs"] 0 0 0 0 [] [] (LineComment [re|^--|] Nothing)
-- >>> findHeader hc $ SourceCode [(Code, "foo"), (Code, "bar"), (Comment, "-- HEADER")]
-- Just (2,2)
findHeader
    :: CtHeaderConfig
    -- ^ appropriate header configuration
    -> SourceCode
    -- ^ text in which to detect the header
    -> Maybe (Int, Int)
    -- ^ header position @(startLine, endLine)@
findHeader HeaderConfig{..} input = case hcHeaderSyntax of
    BlockComment start end _ -> findBlockHeader start end headerArea splitAt
    LineComment prefix _ -> findLineHeader prefix headerArea splitAt
  where
    (before, headerArea, _) = splitSource hcPutAfter hcPutBefore input
    splitAt = length (coerce before :: [CodeLine])

-- | Finds header in the form of /multi-line comment/ syntax, which is delimited
-- with starting and ending pattern.
findBlockHeader
    :: Regex
    -- ^ starting pattern (e.g. @{\-@ or @/*@)
    -> Regex
    -- ^ ending pattern (e.g. @-\}@ or @*/@)
    -> SourceCode
    -- ^ source code in which to detect the header
    -> Int
    -- ^ line number offset (adds to resulting position)
    -> Maybe (Int, Int)
    -- ^ header position @(startLine + offset, endLine + offset)@
findBlockHeader start end sc offset = mapT2 (+ offset) <$> position
  where
    ls = zip [0 ..] $ coerce sc
    isMatch' = \p t -> isMatch p . T.strip $ t
    allComments = all (\(_, (lt, _)) -> lt == Comment)
    hasStart = maybe False (\(_, (_, t)) -> isMatch' start t) . L.headMaybe
    hasEnd = maybe False (\(_, (_, t)) -> isMatch' end t) . L.lastMaybe
    position = (,) <$> (header >>= L.headMaybe) <*> (header >>= L.lastMaybe)
    header =
        (fmap . fmap) fst
            . L.find (\g -> allComments g && hasStart g && hasEnd g)
            . L.groupBy (\(_, (lt1, _)) (_, (lt2, _)) -> lt1 == lt2)
            $ ls

-- | Finds header in the form of /single-line comment/ syntax, which is
-- delimited with the prefix pattern.
--
-- >>> import Headroom.Data.Regex (re)
-- >>> let sc = SourceCode [(Code, ""), (Code, "a"), (Comment, "-- first"), (Comment, "-- second"), (Code, "foo")]
-- >>> findLineHeader [re|^--|] sc 0
-- Just (2,3)
findLineHeader
    :: Regex
    -- ^ prefix pattern (e.g. @--@ or @//@)
    -> SourceCode
    -- ^ source code in which to detect the header
    -> Int
    -- ^ line number offset (adds to resulting position)
    -> Maybe (Int, Int)
    -- ^ header position @(startLine + offset, endLine + offset)@
findLineHeader prefix sc offset = mapT2 (+ offset) <$> position
  where
    ls = zip [0 ..] $ coerce sc
    isMatch' = \p t -> isMatch p . T.strip $ t
    position = (,) <$> (header >>= L.headMaybe) <*> (header >>= L.lastMaybe)
    header =
        (fmap . fmap) fst
            . L.find (all (\(_, (lt, t)) -> lt == Comment && isMatch' prefix t))
            . L.groupBy (\(_, (lt1, _)) (_, (lt2, _)) -> lt1 == lt2)
            $ ls

-- | Splits input source code into three parts:
--
--     1. all lines located before the very last occurence of one of the
--        conditions from the first condition list
--     2. all lines between the first and last lists
--     3. all lines located after the very first occurence of one of the
--        conditions from the second condition list
--
-- If both first and second patterns are empty, then all lines are returned in
-- the middle part.
--
-- >>> import Headroom.Data.Regex (re)
--
-- >>> let ls = [(Code, "text"), (Code, "->"), (Code, "RESULT"), (Code, "<-"), (Code, "foo")]
-- >>> splitSource [[re|->|]] [[re|<-|]] $ SourceCode ls
-- (SourceCode [(Code,"text"),(Code,"->")],SourceCode [(Code,"RESULT")],SourceCode [(Code,"<-"),(Code,"foo")])
--
-- >>> let ls = [(Code, "text"), (Code, "->"), (Code, "RESULT"), (Code, "<-"), (Code, "foo")]
-- >>> splitSource [] [[re|<-|]] $ SourceCode ls
-- (SourceCode [],SourceCode [(Code,"text"),(Code,"->"),(Code,"RESULT")],SourceCode [(Code,"<-"),(Code,"foo")])
--
-- >>> splitSource [] [] $ SourceCode [(Code,"foo"), (Code,"bar")]
-- (SourceCode [],SourceCode [(Code,"foo"),(Code,"bar")],SourceCode [])
splitSource
    :: [Regex]
    -> [Regex]
    -> SourceCode
    -> (SourceCode, SourceCode, SourceCode)
splitSource [] [] sc = (mempty, sc, mempty)
splitSource fstPs sndPs sc = (before, middle, after)
  where
    allLines = coerce sc
    (middle', after) = mapT2 SourceCode $ L.splitAt sndSplit allLines
    (before, middle) = mapT2 SourceCode $ L.splitAt fstSplitAt (coerce middle')
    fstSplitAt = maybe 0 ((+ 1) . fst) $ lastMatching (cond fstPs) middle'
    sndSplit = maybe len fst $ firstMatching (cond sndPs) sc
    len = length allLines
    cond = \ps cl@(lt, t) ->
        if lt == Code && any (`isMatch` t) ps then Just cl else Nothing

mapT2 :: (a -> b) -> (a, a) -> (b, b)
mapT2 = join (***)
