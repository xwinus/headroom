{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.PostProcess
-- Description : Support for /post-processors/
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- /Post-processing functions/ are basically functions that allows to post-process
-- already rendered /license headers/. This is useful to perform some additional
-- operations such as some sort of text alignment, update some parts of the header,
-- etc.
module Headroom.PostProcess
    ( postProcess
    , configuredPostProcess
    , postProcessHeader

      -- * Environment Data Types
    , ConfiguredEnv (..)
    , mkConfiguredEnv
    )
where

import Headroom.Config.Types
    ( CtPostProcessConfigs
    , PostProcessConfig (..)
    , PostProcessConfigs (..)
    , UpdateCopyrightConfig (..)
    )
import Headroom.Data.Has (Has (..))
import Headroom.Data.Lens
    ( suffixLenses
    , suffixLensesFor
    )
import Headroom.PostProcess.Types (PostProcess (..))
import Headroom.PostProcess.UpdateCopyright
    ( SelectedAuthors (..)
    , UpdateCopyrightMode (..)
    , updateCopyright
    )
import Headroom.Template (Template (..))
import Headroom.Template.TemplateRef (TemplateRef (..))
import Headroom.Types (CurrentYear (..))
import Headroom.Variables.Types (Variables (..))
import Lens.Micro (traverseOf)
import RIO

suffixLenses ''PostProcessConfigs
suffixLenses ''UpdateCopyrightConfig
suffixLensesFor ["ppcConfig"] ''PostProcessConfig

-- | Environemnt data type for the composed /post-processor/
-- ('configuredPostProcess').
data ConfiguredEnv = ConfiguredEnv
    { ceCurrentYear :: CurrentYear
    -- ^ current year
    , cePostProcessConfigs :: CtPostProcessConfigs
    -- ^ configuration of /post-processor/
    , ceUpdateCopyrightMode :: UpdateCopyrightMode
    -- ^ mode used by the 'updateCopyright' /post-processor/
    }
    deriving (Eq, Show)

suffixLensesFor ["ceCurrentYear", "ceUpdateCopyrightMode"] ''ConfiguredEnv

instance Has CurrentYear ConfiguredEnv where
    hasLens = ceCurrentYearL

instance Has UpdateCopyrightMode ConfiguredEnv where
    hasLens = ceUpdateCopyrightModeL

-- | Constructor function for 'ConfiguredEnv' data type. This function takes
-- 'Variables' as argument, because it performs template compilation on
-- selected fields of 'CtPostProcessConfigs'.
mkConfiguredEnv
    :: forall a m
     . (Template a, MonadThrow m)
    => CurrentYear
    -- ^ current year
    -> Variables
    -- ^ template variables
    -> CtPostProcessConfigs
    -- ^ configuration for /post-processors/
    -> m ConfiguredEnv
    -- ^ environment data type
mkConfiguredEnv ceCurrentYear vars configs = do
    cePostProcessConfigs <- compileTemplates @a vars configs
    let ceUpdateCopyrightMode = mode cePostProcessConfigs
    pure ConfiguredEnv{..}
  where
    authorsL = ppcsUpdateCopyrightL . ppcConfigL . uccSelectedAuthorsL
    mode = \configs' ->
        maybe
            UpdateAllAuthors
            (UpdateSelectedAuthors . SelectedAuthors)
            (configs' ^. authorsL)

-- | Runs the /post-processing function/ using the given /environment/ and text
-- of rendered /license header/ as input.
postProcess
    :: PostProcess env
    -- ^ /post-processor/ to run
    -> env
    -- ^ environment value
    -> Text
    -- ^ text of rendered /license header/
    -> Text
    -- ^ processed text of /license header/
postProcess (PostProcess fn) env input = runReader (fn input) env

-- | Composition of various /post-processors/, which environment is
-- based on /YAML/ configuration and which can be enabled/disabled to fit
-- end user's needs.
configuredPostProcess
    :: (Has CurrentYear env, Has UpdateCopyrightMode env)
    => CtPostProcessConfigs
    -- ^ configuration of /post-processors/
    -> PostProcess env
    -- ^ composed /post-processor/
configuredPostProcess PostProcessConfigs{..} =
    mconcat
        [ifEnabled ppcsUpdateCopyright updateCopyright]
  where
    ifEnabled PostProcessConfig{..} fn
        | ppcEnabled = fn
        | otherwise = mempty

-- | Takes already rendered /license header/ and post-process it based on the
-- given configuration.
postProcessHeader
    :: ConfiguredEnv
    -- ^ configuration used to define post-processing behaviour
    -> Text
    -- ^ rendered text of /license header/
    -> Text
    -- ^ post-processed text of /license header/
postProcessHeader env =
    postProcess (configuredPostProcess (cePostProcessConfigs env)) env

------------------------------  PRIVATE FUNCTIONS  -----------------------------

compileTemplates
    :: forall a m
     . (Template a, MonadThrow m)
    => Variables
    -> CtPostProcessConfigs
    -> m CtPostProcessConfigs
compileTemplates vars configs = configs & traverseOf authorsL compileAuthors'
  where
    authorsL = ppcsUpdateCopyrightL . ppcConfigL . uccSelectedAuthorsL
    compileAuthors' = mapM . mapM $ compileAuthor
    compileAuthor = \author -> do
        parsed <- parseTemplate @a (InlineRef author) author
        renderTemplate vars parsed
