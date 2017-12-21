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


{-| Converts string between various case forms such as camel case, snake case or kebab case.
-}
convertCase : String -> Bool -> Bool -> String -> String
convertCase separator firstLetterUpper firstLetterOfWordUpper value =
    ""


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
isUpperCase char =
    False


isLetterOrDigit : Char -> Bool
isLetterOrDigit char =
    False


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
split : String -> List String
split val =
    let
        start =
            { machine = Initial
            , firstWord = True
            , firstLetter = True
            , upper = False
            , currentWord = []
            , words = []
            }

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
            case state.machine of
                Initial ->
                    -- state = WordMachineState.StartWord;
                    -- upper = firstLetterOfWordUpper;
                    -- if (!firstWord) {
                    --     result.append(separator);
                    -- }
                    -- firstWord = false;
                    state

                StartWord ->
                    -- state = WordMachineState.ContinueWordCaps;
                    -- upper = false;
                    state

                ContinueWordCaps ->
                    -- state = WordMachineState.ContinueWordCaps;
                    -- upper = false;
                    state

                ContinueWordLower ->
                    -- state = WordMachineState.StartWord;
                    -- upper = firstLetterOfWordUpper;
                    -- result.append(separator);
                    state

        --             writeChar.apply(nextChar, (!firstLetter && upper) || (firstLetter & firstLetterUpper));
        --             firstLetter = false;
        stateTxLetterOrDigit : Char -> State -> State
        stateTxLetterOrDigit char state =
            case state.machine of
                Initial ->
                    -- state = WordMachineState.StartWord;
                    -- upper = firstLetterOfWordUpper;
                    -- if (!firstWord) {
                    --     result.append(separator);
                    -- }
                    -- firstWord = false;
                    state

                StartWord ->
                    -- state = WordMachineState.ContinueWordLower;
                    -- upper = false;
                    state

                ContinueWordCaps ->
                    -- state = WordMachineState.ContinueWordLower;
                    -- upper = false;
                    state

                ContinueWordLower ->
                    -- state = WordMachineState.ContinueWordLower;
                    -- upper = false;
                    state

        --             writeChar.apply(nextChar, (!firstLetter && upper) || (firstLetter & firstLetterUpper));
        --             firstLetter = false;
        stateTxWhitespace : Char -> State -> State
        stateTxWhitespace char state =
            case state.machine of
                Initial ->
                    -- state = WordMachineState.Initial;
                    state

                StartWord ->
                    -- state = WordMachineState.Initial;
                    state

                ContinueWordCaps ->
                    -- state = WordMachineState.Initial;
                    state

                ContinueWordLower ->
                    -- state = WordMachineState.Initial;
                    state

        --             upper = false;
    in
        List.foldl (\char -> \state -> state) start (String.toList val)
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
