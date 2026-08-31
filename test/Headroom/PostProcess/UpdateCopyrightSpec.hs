{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.PostProcess.UpdateCopyrightSpec
    ( spec
    )
where

import Headroom.Data.Has (Has (..))
import Headroom.Data.Text (fromLines)
import Headroom.PostProcess (postProcess)
import Headroom.PostProcess.UpdateCopyright
import Headroom.Types (CurrentYear (..))
import RIO
import Test.Hspec

spec :: Spec
spec = do
    let currYear = CurrentYear 2020

    describe "updateCopyright" $ do
        it "updates all authors when such mode selected" $ do
            let sample =
                    fromLines
                        [ "Copyright (c) 2019 1st Author"
                        , "Copyright (c) 2017-2019 2nd Author"
                        ]
                expected =
                    fromLines
                        [ "Copyright (c) 2019-2020 1st Author"
                        , "Copyright (c) 2017-2020 2nd Author"
                        ]
                testEnv = TestEnv currYear UpdateAllAuthors
            postProcess updateCopyright testEnv sample `shouldBe` expected

        it "updates only selected authors in such mode" $ do
            let sample =
                    fromLines
                        [ "Copyright (c) 2019 1st Author"
                        , "Copyright (c) 2017-2019 2nd Author"
                        ]
                expected =
                    fromLines
                        [ "Copyright (c) 2019 1st Author"
                        , "Copyright (c) 2017-2020 2nd Author"
                        ]
                mode = UpdateSelectedAuthors . SelectedAuthors $ "2nd Author" :| []
                testEnv = TestEnv currYear mode
            postProcess updateCopyright testEnv sample `shouldBe` expected

        it "matches selected authors only in parsed author text" $ do
            let sample = "// 2nd Author | Copyright (c) 2019 1st Author"
                mode = UpdateSelectedAuthors . SelectedAuthors $ "2nd Author" :| []
                testEnv = TestEnv currYear mode
            postProcess updateCopyright testEnv sample `shouldBe` sample

        it "preserves the GPL2 postal code in all-authors mode" $ do
            let sample =
                    fromLines
                        [ " * Copyright (c) 2019 Example Author"
                        , " * Foundation, Inc., 59 Temple Place, Suite 330,"
                        , " * Boston, MA 02111-1307"
                        ]
                expected =
                    fromLines
                        [ " * Copyright (c) 2019-2020 Example Author"
                        , " * Foundation, Inc., 59 Temple Place, Suite 330,"
                        , " * Boston, MA 02111-1307"
                        ]
                testEnv = TestEnv currYear UpdateAllAuthors
            postProcess updateCopyright testEnv sample `shouldBe` expected

    describe "updateYears" $ do
        it "does nothing on up-to-date year" $ do
            let sample = "Copyright (c) 2020"
            updateYears currYear sample `shouldBe` sample

        it "does nothing if year is higher than current year" $ do
            let sample = "Copyright (c) 2021"
            updateYears currYear sample `shouldBe` sample

        it "does nothing on up-to-date year range" $ do
            let sample = "Copyright (c) 2018-2020"
            updateYears currYear sample `shouldBe` sample

        it "does nothing if second year range is higher than current year" $ do
            let sample = "Copyright (c) 2018-2021"
            updateYears currYear sample `shouldBe` sample

        it "does nothing if entire year range is higher than current year" $ do
            let sample = "Copyright (c) 2021-2023"
            updateYears currYear sample `shouldBe` sample

        it "updates outdated year" $ do
            let sample = "Copyright (c) 2019"
                expected = "Copyright (c) 2019-2020"
            updateYears currYear sample `shouldBe` expected

        it "updates outdated year range" $ do
            let sample = "Copyright (c) 2017-2019"
                expected = "Copyright (c) 2017-2020"
            updateYears currYear sample `shouldBe` expected

        it "preserves unrelated numeric identifiers in a statement" $ do
            let sample =
                    "Copyright (c) 2019 Example, version 1234, https://example.com/v2"
                expected =
                    "Copyright (c) 2019-2020 Example, version 1234, https://example.com/v2"
            updateYears currYear sample `shouldBe` expected

        it "preserves malformed and ambiguous statements" $ do
            let samples =
                    [ "Copyright (c) 2019-2020-2021 Example"
                    , "Copyright (c) 2019 2020 Example"
                    , "Copyright notice Copyright (c) 2019 Example"
                    , "Copyright (c) 2020-2019 Example"
                    , "Copyright (c) 0211-1307 Example"
                    ]
            updateYears currYear <$> samples `shouldBe` samples

        it "updates complex multi-line text" $ do
            let sample =
                    fromLines
                        [ "Copyright (c) 2019"
                        , "Copyright (c) 2020"
                        , "Copyright (c) 2019-2020"
                        , "Copyright (c) 2017-2019"
                        ]
                expected =
                    fromLines
                        [ "Copyright (c) 2019-2020"
                        , "Copyright (c) 2020"
                        , "Copyright (c) 2019-2020"
                        , "Copyright (c) 2017-2020"
                        ]
            updateYears currYear sample `shouldBe` expected

-------------------------------  TEST DATA TYPES  ------------------------------

data TestEnv = TestEnv
    { teCurrentYear :: CurrentYear
    , teMode :: UpdateCopyrightMode
    }
    deriving (Eq, Show)

instance Has CurrentYear TestEnv where
    hasLens = lens teCurrentYear (\x y -> x{teCurrentYear = y})

instance Has UpdateCopyrightMode TestEnv where
    hasLens = lens teMode (\x y -> x{teMode = y})
