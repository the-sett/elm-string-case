module String.Case exposing (..)

{-| String functions that are useful when working with source code.
-}


{-| Used to track the state of the machine that extracts words from variable names.
-}
type WordMachineState
    = Initial
    | StartWord
    | ContinueWordCaps
    | ContinueWordLower


{-| Converts string between various case forms such as camel case, snake case or kebab case.
-}
convertCase : String -> String -> Bool -> Bool -> String
convertCase value separator firstLetterUpper firstLetterOfWordUpper =
    ""


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



--     boolean firstWord = true;
--     boolean firstLetter = true;
--     boolean upper = false;
--     WordMachineState state = WordMachineState.Initial;
--
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
--
--     for (int i = 0; i < value.length(); i++) {
--         char nextChar = value.charAt(i);
--
--         if (Character.isUpperCase(nextChar)) {
--             switch (state) {
--                 case Initial:
--                     state = WordMachineState.StartWord;
--                     upper = firstLetterOfWordUpper;
--                     if (!firstWord) {
--                         result.append(separator);
--                     }
--                     firstWord = false;
--                     break;
--                 case StartWord:
--                 case ContinueWordCaps:
--                     state = WordMachineState.ContinueWordCaps;
--                     upper = false;
--                     break;
--                 case ContinueWordLower:
--                     state = WordMachineState.StartWord;
--                     upper = firstLetterOfWordUpper;
--                     result.append(separator);
--                     break;
--             }
--
--             writeChar.apply(nextChar, (!firstLetter && upper) || (firstLetter & firstLetterUpper));
--             firstLetter = false;
--         } else if (Character.isLetterOrDigit(nextChar)) {
--             switch (state) {
--                 case Initial:
--                     state = WordMachineState.StartWord;
--                     upper = firstLetterOfWordUpper;
--                     if (!firstWord) {
--                         result.append(separator);
--                     }
--                     firstWord = false;
--                     break;
--                 case StartWord:
--                 case ContinueWordLower:
--                 case ContinueWordCaps:
--                     state = WordMachineState.ContinueWordLower;
--                     upper = false;
--                     break;
--             }
--
--             writeChar.apply(nextChar, (!firstLetter && upper) || (firstLetter & firstLetterUpper));
--             firstLetter = false;
--         } else {
--             switch (state) {
--                 case Initial:
--                     state = WordMachineState.Initial;
--                     break;
--                 case StartWord:
--                 case ContinueWordCaps:
--                 case ContinueWordLower:
--                     state = WordMachineState.Initial;
--                     break;
--             }
--
--             upper = false;
--         }
--     }
--
--     return result.toString();


{-| Converts a string to camel case with the first letter in uppercase.
-}
toCamelCaseUpper name =
    convertCase name "" True True


{-| Converts a string to camel case with the first letter in lowercase.
-}
toCamelCaseLower name =
    convertCase name "" False True


{-| Converts a string to snake case with the first letter in uppercase.
-}
toSnakeCaseUpper name =
    convertCase name "_" True True


{-| Converts a string to snake case with the first letter in lowercase.
-}
toSnakeCaseLower name =
    convertCase name "_" False False


{-| Converts a string to kebab case with the first letter in uppercase.
-}
toKebabCaseUpper name =
    convertCase name "-" True True


{-| Converts a string to kebab case with the first letter in lowercase.
-}
toKebabCaseLower name =
    convertCase name "-" False False
