module String.Case
    exposing
        ( convertCase
        , toCamelCaseUpper
        , toCamelCaseLower
        , toSnakeCaseUpper
        , toSnakeCaseLower
        , toKebabCaseUpper
        , toKebabCaseLower
        )

{-| String functions that are useful when working with source code.
@docs convertCase, toCamelCaseUpper, toCamelCaseLower, toSnakeCaseUpper
@docs toSnakeCaseLower, toKebabCaseUpper, toKebabCaseLower
-}

import Char


{-| Converts string between various case forms such as camel case, snake case or kebab case.
Strings are split into words on capital letters or whitespace.
For example, to convert to space case with the first letter of the first word capitalized and
the first letters of subsequent words in lower case:

    (convertCase " " True False "convertToSpaceCase") == "Convert to space case"

-}
convertCase : String -> Bool -> Bool -> String -> String
convertCase separator firstLetterUpper firstLetterOfWordUpper value =
    split firstLetterUpper firstLetterOfWordUpper value
        |> List.reverse
        |> List.intersperse separator
        |> String.concat


{-| Used to track the state of the machine that extracts words from variable names.
-}
type alias State =
    { machine : WordMachineState
    , firstWord : Bool
    , firstLetter : Bool
    , upper : Bool
    , currentWord : List Char
    , words : List String
    }


{-| Used to encode the state of the word machine.
-}
type WordMachineState
    = Initial
    | StartWord
    | ContinueWordCaps
    | ContinueWordLower



--     Function2<Character, Boolean, StringBuffer> writeChar = new Function2<Character, Boolean, StringBuffer>() {
--         public StringBuffer apply(Character nextChar, Boolean upper) {
--             if (upper)
--                 result.append(Character.toUpperCase(nextChar));
--             else
--                 result.append(Character.toLowerCase(nextChar));
--
--             return result;
--         }
--     };


isUpperCase : Char -> Bool
isUpperCase =
    Char.isUpper


isLetterOrDigit : Char -> Bool
isLetterOrDigit char =
    Char.isUpper char || Char.isLower char || Char.isDigit char


{-| Splits a string into a list of words.
Characters within the string are characterized as whitespace, upper case, or
other by the following rules.

1.  An upper case letter is marked as upper case (U).
2.  A lower case letter or digit is marked as lower case (L).
3.  Any other character is marked as whitespace (W).

The string is processed into words by these rules.

1.  W at the start is discarded.
2.  U or L begins a word.
3.  U after the initial character of a word continues the same word, so long as no W or L is encountered.
4.  L after U or L continues the word, so long as no W is encountered.
5.  U after L begins a new word.
6.  W after U or L is discarded and ends the current word.

-}
split : Bool -> Bool -> String -> List String
split firstLetterUpper firstLetterOfWordUpper value =
    let
        start =
            { machine = Initial
            , firstWord = True
            , firstLetter = True
            , upper = False
            , currentWord = []
            , words = []
            }

        -- Conditionally word breaks at the current character
        -- The state is modified with the current word appended onto the output
        -- and the current word cleared to begin a new one, when the condition
        -- flag is set to True.
        wordBreak : Bool -> State -> State
        wordBreak condition state =
            if condition then
                { state | words = (String.fromList <| List.reverse state.currentWord) :: state.words, currentWord = [] }
            else
                { state | words = state.words, currentWord = state.currentWord }

        -- Appends a character to the current word, in upper or lower case.
        writeChar : Char -> State -> State
        writeChar char state =
            if ((not state.firstLetter && state.upper) || (state.firstLetter && firstLetterUpper)) then
                { state | currentWord = Char.toUpper char :: state.currentWord }
            else
                { state | currentWord = Char.toLower char :: state.currentWord }

        stateFn : Char -> State -> State
        stateFn char state =
            if isUpperCase char then
                stateTxUpperCase char state
            else if isLetterOrDigit char then
                stateTxLetterOrDigit char state
            else
                stateTxWhitespace char state

        stateTxUpperCase : Char -> State -> State
        stateTxUpperCase char state =
            (case state.machine of
                Initial ->
                    { state
                        | machine = StartWord
                        , upper = firstLetterOfWordUpper
                        , firstWord = False
                    }
                        |> wordBreak (not state.firstWord)

                StartWord ->
                    { state
                        | machine = ContinueWordCaps
                        , upper = False
                    }

                ContinueWordCaps ->
                    { state
                        | machine = ContinueWordCaps
                        , upper = False
                    }

                ContinueWordLower ->
                    { state
                        | machine = StartWord
                        , upper = firstLetterOfWordUpper
                    }
                        |> wordBreak True
            )
                |> writeChar char
                |> (\state -> { state | firstLetter = False })

        stateTxLetterOrDigit : Char -> State -> State
        stateTxLetterOrDigit char state =
            (case state.machine of
                Initial ->
                    { state
                        | machine = StartWord
                        , upper = firstLetterOfWordUpper
                        , firstWord = False
                    }
                        |> wordBreak (not state.firstWord)

                StartWord ->
                    { state
                        | machine = ContinueWordLower
                        , upper = False
                    }

                ContinueWordCaps ->
                    { state
                        | machine = ContinueWordLower
                        , upper = False
                    }

                ContinueWordLower ->
                    { state
                        | machine = ContinueWordLower
                        , upper = False
                    }
            )
                |> writeChar char
                |> (\state -> { state | firstLetter = False })

        stateTxWhitespace : Char -> State -> State
        stateTxWhitespace char state =
            { state
                | machine = Initial
                , upper = False
            }

        -- Appends the last (current) word if there is one.
        appendLastWord : State -> State
        appendLastWord state =
            if state.currentWord == [] then
                state
            else
                wordBreak True state
    in
        List.foldl (\char -> \state -> stateFn char state) start (String.toList value)
            |> appendLastWord
            |> .words


{-| Converts a string to camel case with the first letter in uppercase.
-}
toCamelCaseUpper : String -> String
toCamelCaseUpper name =
    convertCase "" True True name


{-| Converts a string to camel case with the first letter in lowercase.
-}
toCamelCaseLower : String -> String
toCamelCaseLower name =
    convertCase "" False True name


{-| Converts a string to snake case with the first letter in uppercase.
-}
toSnakeCaseUpper : String -> String
toSnakeCaseUpper name =
    convertCase "_" True True name


{-| Converts a string to snake case with the first letter in lowercase.
-}
toSnakeCaseLower : String -> String
toSnakeCaseLower name =
    convertCase "_" False False name


{-| Converts a string to kebab case with the first letter in uppercase.
-}
toKebabCaseUpper : String -> String
toKebabCaseUpper name =
    convertCase "-" True True name


{-| Converts a string to kebab case with the first letter in lowercase.
-}
toKebabCaseLower : String -> String
toKebabCaseLower name =
    convertCase "-" False False name
