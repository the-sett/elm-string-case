module CaseTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import String.Case exposing (..)


testCases =
    [ [ "test_string", "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ]
    , [ "test__string", "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ]
    , [ "a_test_string", "aTestString", "ATestString", "a_test_string", "A_Test_String", "a-test-string", "A-Test-String" ]
    , [ "_test", "test", "Test", "test", "Test", "test", "Test" ]
    , [ "test_", "test", "Test", "test", "Test", "test", "Test" ]
    , [ "testString", "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ]
    , [ "testString", "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ]
    , [ "test-string", "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ]
    , [ "test-string", "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ]
    ]


suite : Test
suite =
    todo "Implement our first test. See http://package.elm-lang.org/packages/elm-community/elm-test/latest for how to do this!"
