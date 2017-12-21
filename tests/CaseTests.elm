module CaseTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import String.Case exposing (..)
import List.Extra as LE


type alias TestCases =
    ( List ( String, String -> String ), List ( String, List String ) )


caseTestCases : TestCases
caseTestCases =
    ( [ ( "toCamelCaseLower", toCamelCaseLower )
      , ( "toCamelCaseUpper", toCamelCaseUpper )
      , ( "toSnakeCaseLower", toSnakeCaseLower )
      , ( "toSnakeCaseUpper", toSnakeCaseUpper )
      , ( "toKebabCaseLower", toKebabCaseLower )
      , ( "toKebabCaseUpper", toKebabCaseUpper )
      ]
    , [ ( "test_string", [ "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ] )
      , ( "test__string", [ "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ] )
      , ( "a_test_string", [ "aTestString", "ATestString", "a_test_string", "A_Test_String", "a-test-string", "A-Test-String" ] )
      , ( "_test", [ "test", "Test", "test", "Test", "test", "Test" ] )
      , ( "test_", [ "test", "Test", "test", "Test", "test", "Test" ] )
      , ( "testString", [ "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ] )
      , ( "TestString", [ "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ] )
      , ( "test-string", [ "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ] )
      , ( "Test-String", [ "testString", "TestString", "test_string", "Test_String", "test-string", "Test-String" ] )
      ]
    )


wordSplitTestCases : TestCases
wordSplitTestCases =
    ( [ ( "spaceCase", convertCase " " False False ) ]
    , [ ( "", [ "" ] )
      , ( "word", [ "word" ] )
      , ( " word", [ "word" ] )
      , ( "-word", [ "word" ] )
      , ( "_word", [ "word" ] )
      , ( "--word", [ "word" ] )
      , ( "_1word", [ "1word" ] )
      , ( "word ", [ "word" ] )
      , ( "word-", [ "word" ] )
      , ( "word_", [ "word" ] )
      , ( "_word1_word2", [ "word1 word2" ] )
      , ( "word1Word2", [ "word1 word2" ] )
      , ( "GDPuk", [ "gdpuk" ] )
      , ( "GDP-uk", [ "gdp uk" ] )
      ]
    )


buildTests : TestCases -> List Test
buildTests ( namedFns, cases ) =
    List.map
        (\( input, outputs ) ->
            List.map (\output -> ( input, output ))
                outputs
        )
        cases
        |> List.map (\ioPairs -> LE.zip namedFns ioPairs)
        |> List.concat
        |> List.map (\( ( name, fn ), ( input, output ) ) -> testConvert name fn input output)


testConvert : String -> (String -> String) -> String -> String -> Test
testConvert name convertFn input output =
    test ("Check conversion of " ++ input ++ " to " ++ output ++ " by " ++ name) <|
        \_ ->
            Expect.equal output <| convertFn input


suite : Test
suite =
    describe "The String.Case module conversion functions."
        [ describe "Word splitting tests." <|
            buildTests wordSplitTestCases
        , describe "Case conversion tests." <|
            buildTests caseTestCases
        ]
