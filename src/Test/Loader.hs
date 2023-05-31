{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Loader (
    allTests,
    test_Applicative,
    test_Functor,
    test_List,
    test_Monad,
    test_Optional,
    test_State,
    test_StateT,
    test,
)
where

import Test.ApplicativeTest (test_Applicative)
import Test.FunctorTest (test_Functor)
import Test.ListTest (test_List)
import Test.MonadTest (test_Monad)
import Test.OptionalTest (test_Optional)
import Test.StateTest (test_State)
import Test.StateTTest (test_StateT)

import Data.String (fromString)

import Test.Framework (TestTree, test, testGroup)

allTests :: TestTree
allTests =
    testGroup
        "Tests"
        [ test_Optional
        , test_List
        , test_Functor
        , test_Applicative
        , test_Monad
        , test_State
        , test_StateT
        ]
