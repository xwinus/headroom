{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Template.MustacheSpec
    ( spec
    )
where

import Headroom.Template
import Headroom.Template.Mustache
import Headroom.Template.TemplateRef (TemplateRef (..))
import Headroom.Variables (mkVariables)
import RIO
import Test.Hspec
import qualified Text.Mustache.Types as MU

spec :: Spec
spec = do
    describe "parseTemplate" $ do
        it "parses Mustache template from raw text" $ do
            let template = "Hello, {{ name }}"
                parsed = parseTemplate @Mustache (InlineRef template) template
            parsed `shouldSatisfy` isJust

    describe "renderTemplate" $ do
        it "renders template with given variables" $ do
            let template = "Hello, {{ name }}"
                variables = mkVariables [("name", "John")]
                parsed = parseTemplate @Mustache (InlineRef template) template
                rendered = parsed >>= renderTemplate variables
            rendered `shouldBe` Just "Hello, John"

        it "fails if not enough variables is provided" $ do
            let template = "Hello, {{ name }} {{ surname }}"
                variables = mkVariables [("name", "John")]
                parsed = parseTemplate @Mustache (InlineRef template) template
            let err (MissingVariables "<inline template 'Hello, {{ name }} {{ surname }}'>" ["surname"]) =
                    True
                err _ = False
            (parsed >>= renderTemplate variables) `shouldThrow` err

        it "reports the complete path of a missing dotted variable" $ do
            let template = "Hello, {{ user.name }}"
                parsed = parseTemplate @Mustache (InlineRef template) template
                err (MissingVariables _ ["user.name"]) = True
                err _ = False

            (parsed >>= renderTemplate mempty) `shouldThrow` err

        it "reports multiple simple and dotted missing variables in order" $ do
            let template = "{{ user.name }} {{ company }} {{ address.city }}"
                parsed = parseTemplate @Mustache (InlineRef template) template
                err (MissingVariables _ ["user.name", "company", "address.city"]) = True
                err _ = False

            (parsed >>= renderTemplate mempty) `shouldThrow` err

        it "renders a valid dotted variable" $ do
            let template = "Hello, {{ user.name }}"
                parsed = parseTemplate @Mustache (InlineRef template) template
                variables =
                    MU.object
                        [ "user" MU.~> MU.object ["name" MU.~> ("John" :: Text)]
                        ]
                rendered = parsed >>= renderMustache variables

            rendered `shouldBe` Just "Hello, John"

        it "renders template with conditionally set variable" $ do
            let template = "Foo {{#bar}}{{bar}}{{/bar}}{{^bar}}BAR{{/bar}}"
                variables = mempty
                parsed = parseTemplate @Mustache (InlineRef template) template
                rendered = parsed >>= renderTemplate variables
            rendered `shouldBe` Just "Foo BAR"

        it "fails if non-existing variable is used with inverted sections" $ do
            let template = "Foo {{bar}}{{^bar}}BAR{{/bar}}"
                variables = mkVariables [("xx", "yy")]
                parsed = parseTemplate @Mustache (InlineRef template) template
                rendered = parsed >>= renderTemplate variables
            rendered `shouldBe` Nothing

    describe "rawTemplate" $ do
        it "returns raw template text for already parsed template" $ do
            let template = "Hello, {{ name }}"
                parsed = parseTemplate @Mustache (InlineRef template) template
            fmap rawTemplate parsed `shouldBe` Just template
