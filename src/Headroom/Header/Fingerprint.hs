{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Header.Fingerprint
-- Description : Conservative content fingerprints for license headers
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Matches source comments against rendered license header content without
-- adding ownership metadata to the processed files.
module Headroom.Header.Fingerprint
    ( matchHeader
    )
where

import Data.Char
    ( isAlphaNum
    , isDigit
    )
import Headroom.Config.Types (HeaderSyntax (..))
import Headroom.Header.Sanitize (stripCommentSyntax)
import Headroom.Header.Types (HeaderOrigin (..))
import RIO
import qualified RIO.List as L
import qualified RIO.Set as S
import qualified RIO.Text as T

-- | Finds a high-confidence prefix matching the rendered template. Line
-- headers must occupy the same number of lines as the rendered template, which
-- prevents adjacent documentation comments from becoming part of the match.
matchHeader :: HeaderSyntax -> Text -> [Text] -> Maybe (HeaderOrigin, Int)
matchHeader syntax expected candidateLines =
    exactMatch <|> fingerprintMatch
  where
    sizes = case syntax of
        LineComment{}
            | length candidateLines >= expectedLineCount -> [expectedLineCount]
            | otherwise -> []
        BlockComment{} -> [length candidateLines]
    candidates = fmap candidate sizes
    candidate size = (size, normalize . T.intercalate "\n" $ take size candidateLines)
    normalizedExpected = normalize expected
    expectedLineCount = length $ T.lines expected
    exactMatch = (\(size, _) -> (ExactTemplate, size)) <$> L.headMaybe exactCandidates
    exactCandidates = filter ((== normalizedExpected) . snd) candidates
    fingerprintMatch = do
        (size, _) <- bestFingerprint $ mapMaybe fingerprint candidates
        pure (TemplateFingerprint, size)
    fingerprint (size, normalizedCandidate)
        | isStrongFingerprint normalizedExpected normalizedCandidate =
            Just (size, similarity normalizedExpected normalizedCandidate)
        | otherwise = Nothing
    normalize = normalizeText . stripCommentSyntax syntaxWithoutPrefix
    syntaxWithoutPrefix = case syntax of
        BlockComment start end _ -> BlockComment start end Nothing
        LineComment start _ -> LineComment start Nothing

bestFingerprint :: [(Int, Double)] -> Maybe (Int, Double)
bestFingerprint = foldl' choose Nothing
  where
    choose Nothing candidate = Just candidate
    choose current@(Just old) candidate
        | fingerprintKey candidate > fingerprintKey old = Just candidate
        | otherwise = current
    fingerprintKey (size, score) = (score, negate size)

isStrongFingerprint :: Text -> Text -> Bool
isStrongFingerprint expected candidate =
    matchingSpdx || matchingLicenseBody
  where
    score = similarity expected candidate
    matchingSpdx =
        score
            >= minimumSpdxSimilarity
            && not (null expectedSpdx)
            && expectedSpdx
            == candidateSpdx
    matchingLicenseBody =
        S.size (tokens expected)
            >= minimumLicenseTokenCount
            && score
            >= minimumLicenseSimilarity
            && alignedLineSimilarity expected candidate
            >= minimumAlignedLines
    expectedSpdx = spdxLines expected
    candidateSpdx = spdxLines candidate

minimumSpdxSimilarity :: Double
minimumSpdxSimilarity = 0.5

minimumLicenseTokenCount :: Int
minimumLicenseTokenCount = 20

minimumLicenseSimilarity :: Double
minimumLicenseSimilarity = 0.72

minimumAlignedLines :: Double
minimumAlignedLines = 0.5

similarity :: Text -> Text -> Double
similarity left right
    | S.null union = 0
    | otherwise = fromIntegral (S.size intersection) / fromIntegral (S.size union)
  where
    leftTokens = tokens left
    rightTokens = tokens right
    intersection = S.intersection leftTokens rightTokens
    union = S.union leftTokens rightTokens

alignedLineSimilarity :: Text -> Text -> Double
alignedLineSimilarity left right
    | total == 0 = 0
    | otherwise = fromIntegral matching / fromIntegral total
  where
    leftLines = normalizedLines left
    rightLines = normalizedLines right
    matching = length . filter (uncurry (==)) $ zip leftLines rightLines
    total = max (length leftLines) (length rightLines)

tokens :: Text -> Set Text
tokens =
    S.fromList
        . filter (\token -> T.length token > 2 && not (T.all isDigit token))
        . T.words
        . normalizeText

spdxLines :: Text -> [Text]
spdxLines = filter ("spdx license identifier" `T.isInfixOf`) . T.lines

normalizeText :: Text -> Text
normalizeText = T.unlines . normalizedLines

normalizedLines :: Text -> [Text]
normalizedLines =
    filter (not . T.null)
        . fmap (T.unwords . T.words . T.map normalizeCharacter . T.toLower)
        . T.lines
        . T.strip
  where
    normalizeCharacter character
        | isAlphaNum character = character
        | otherwise = ' '
