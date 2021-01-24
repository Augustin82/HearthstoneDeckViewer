module Utils exposing (..)

import Element exposing (..)
import Html
import Html.Events as HE
import Json.Decode as Decode
import Regex exposing (Regex)


makeDarker : Float -> Color -> Color
makeDarker r color =
    let
        ratio =
            1 - r

        { red, green, blue, alpha } =
            toRgb color

        darkerRed =
            red * ratio

        darkerGreen =
            green * ratio

        darkerBlue =
            blue * ratio
    in
    rgba darkerRed darkerGreen darkerBlue alpha


makeLighter : Float -> Color -> Color
makeLighter r color =
    let
        ratio =
            1 + r

        { red, green, blue, alpha } =
            toRgb color

        lighterRed =
            red * ratio

        lighterGreen =
            green * ratio

        lighterBlue =
            blue * ratio
    in
    rgba lighterRed lighterGreen lighterBlue alpha


makeOpaque : Float -> Color -> Color
makeOpaque opacity color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgba red green blue opacity


replaceLigatureAndDiacriticReplacer : Regex.Match -> String
replaceLigatureAndDiacriticReplacer { match } =
    case match of
        "Á" ->
            "A"

        "À" ->
            "A"

        "Ä" ->
            "A"

        "Â" ->
            "A"

        "á" ->
            "a"

        "à" ->
            "a"

        "ä" ->
            "a"

        "â" ->
            "a"

        "É" ->
            "E"

        "È" ->
            "E"

        "Ë" ->
            "E"

        "Ê" ->
            "E"

        "é" ->
            "e"

        "è" ->
            "e"

        "ë" ->
            "e"

        "ê" ->
            "e"

        "Í" ->
            "I"

        "Ì" ->
            "I"

        "Ï" ->
            "I"

        "Î" ->
            "I"

        "í" ->
            "i"

        "ì" ->
            "i"

        "ï" ->
            "i"

        "î" ->
            "i"

        "Ó" ->
            "O"

        "Ò" ->
            "O"

        "Ö" ->
            "O"

        "Ô" ->
            "O"

        "ó" ->
            "o"

        "ò" ->
            "o"

        "ö" ->
            "o"

        "ô" ->
            "o"

        "Ú" ->
            "U"

        "Ù" ->
            "U"

        "Ü" ->
            "U"

        "Û" ->
            "U"

        "ú" ->
            "u"

        "ù" ->
            "u"

        "ü" ->
            "u"

        "û" ->
            "u"

        "Ý" ->
            "Y"

        "Ỳ" ->
            "Y"

        "Ÿ" ->
            "Y"

        "Ŷ" ->
            "Y"

        "ý" ->
            "y"

        "ỳ" ->
            "y"

        "ÿ" ->
            "y"

        "ŷ" ->
            "y"

        "Ç" ->
            "C"

        "ç" ->
            "c"

        "Œ" ->
            "OE"

        "œ" ->
            "oe"

        "Æ" ->
            "AE"

        "æ" ->
            "ae"

        _ ->
            match


frenchDiacriticsAndLigaturesRegexMatcher : Regex
frenchDiacriticsAndLigaturesRegexMatcher =
    Regex.fromString "[ÁÀÄÂáàäâÉÈËÊéèëêÍÌÏÎíìïîÓÒÖÔóòöôÚÙÜÛúùüûÝỲŸŶýỳÿŷÇçŒœÆæ]"
        |> Maybe.withDefault Regex.never


toFrenchAlphabetical : String -> String
toFrenchAlphabetical =
    Regex.replace frenchDiacriticsAndLigaturesRegexMatcher replaceLigatureAndDiacriticReplacer


alphanumericalLowerCaseFrenchDiacriticsCompare : String -> String -> Order
alphanumericalLowerCaseFrenchDiacriticsCompare a b =
    let
        string1 =
            a
                |> toFrenchAlphabetical

        string2 =
            b
                |> toFrenchAlphabetical
    in
    compare string1 string2


frenchCompare : String -> String -> Order
frenchCompare =
    alphanumericalLowerCaseFrenchDiacriticsCompare


frenchContains : String -> String -> Bool
frenchContains needle haystack =
    haystack
        |> toFrenchAlphabetical
        |> String.toLower
        |> String.contains
            (needle
                |> toFrenchAlphabetical
                |> String.toLower
            )


frenchTableSort : (data -> String) -> data -> String
frenchTableSort toString =
    toString
        >> toFrenchAlphabetical
        >> String.toLower


onEnter : msg -> Html.Attribute msg
onEnter tagger =
    HE.on "keyup"
        (HE.keyCode
            |> Decode.andThen
                (\key ->
                    if key == 13 then
                        Decode.succeed tagger

                    else
                        Decode.fail "Ignoring, not 'Enter'!"
                )
        )


{-| Trim the whitespace of both sides of the string and compress
repeated whitespace internally to a single whitespace char.
clean " The quick brown fox " == "The quick brown fox"
taken from <https://github.com/elm-community/string-extra/> v4.0.1
-}
clean : String -> String
clean string =
    string
        |> Regex.replace (regexFromString "\\s\\s+") (always " ")
        |> String.trim


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never
