module Main exposing (main)

import Browser
import Element
import Element.Input as Input
import Element.Region as Region
import Html exposing (..)
import Html.Attributes exposing (..)



-- node "!DOCTYPE" [ attribute "html" "" ]
--     [ node "html" []
--         [ node "head" []
--             [ node "title" []
--                 [ text "Hearthstone Deck Viewer" ]
--             , node "meta" [ content "A simple tool for viewing and sharing Hearthstone decks and deck lineups.", name "description" ]
--                 []
--             , node "meta" [ content "Hearthstone,deck,deckstring,decklist", name "keywords" ]
--                 []
--             , node "meta" [ charset "utf-8" ]
--                 []
--             , node "meta" [ content "width=device-width, initial-scale=1, shrink-to-fit=no", name "viewport" ]
--                 []
--             , node "link" [ attribute "crossorigin" "anonymous", href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css", attribute "integrity" "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm", rel "stylesheet" ]
--                 []
--             , node "link" [ href "css/styles.css", rel "stylesheet" ]
--                 []
--             , node "script" [ src "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.min.js" ]
--                 []
--             , node "script" [ attribute "crossorigin" "anonymous", attribute "integrity" "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8=", src "https://code.jquery.com/jquery-3.3.1.min.js" ]
--                 []
--             , node "script" [ attribute "crossorigin" "anonymous", attribute "integrity" "sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q", src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" ]
--                 []
--             , node "script" [ attribute "crossorigin" "anonymous", attribute "integrity" "sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl", src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" ]
--                 []
--             , node "script" [ src "js/bundle.js" ]
--                 []
--             ]
--         , body []


type alias Flags =
    ()


type alias Model =
    { pasted : String
    }


type Msg
    = NoOp
    | UpdateInput String
    | AddDecks


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { pasted = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput string ->
            ( { model | pasted = string }, Cmd.none )

        AddDecks ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view { pasted } =
    { title = "Elm version of HS Deck Viewer"
    , body =
        [ Element.layout [] <|
            Element.column [] <|
                [ Element.column [ Element.htmlAttribute <| id "header-section" ]
                    [ Element.el [ Region.heading 1 ] <| Element.text "Hearthstone Deck Viewer"
                    , Element.row []
                        [ Input.text
                            [ Element.htmlAttribute <| id "deckstring"
                            , Element.htmlAttribute <| name "deckstring"
                            ]
                            { placeholder =
                                Just <|
                                    Input.placeholder [] <|
                                        Element.text "Input deck code(s) here"
                            , label = Input.labelHidden ""
                            , onChange = UpdateInput
                            , text = pasted
                            }
                        , Input.button
                            [ Element.htmlAttribute <| id "addButton"
                            , Element.htmlAttribute <| class "btn btn-outline-light"
                            ]
                            { onPress = Just AddDecks
                            , label = Element.text "Add Deck(s)"
                            }
                        ]
                    , Element.el [ Element.htmlAttribute <| class "form-text text-muted mb-2" ] <|
                        Element.text "Separate multiple deck codes with whitespace or commas. Individual deck strings copied from the game client are also supported."
                    , Element.row [ Element.htmlAttribute <| id "shortURLForm" ]
                        [ Input.button
                            [ Element.htmlAttribute <| id "urlButton"
                            , Element.htmlAttribute <| class "btn btn-outline-light"
                            ]
                            { onPress = Nothing
                            , label = Element.text "Generate Short URL"
                            }
                        , Input.text
                            [ Element.htmlAttribute <|
                                id "urlInput"
                            , Element.htmlAttribute <| name "url"
                            ]
                            { onChange = always NoOp
                            , placeholder = Nothing
                            , label = Input.labelHidden ""
                            , text = ""
                            }
                        , Input.button
                            [ Element.htmlAttribute <| id "copyButton"
                            , Element.htmlAttribute <| class "btn btn-outline-light"
                            ]
                            { onPress = Nothing
                            , label = Element.image [ Element.htmlAttribute <| class "clippy" ] { src = "images/clippy.svg", description = "Copy to clipboard" }
                            }
                        ]
                    , Input.button
                        [ Element.htmlAttribute <| id "removeButton"
                        , Element.htmlAttribute <| class "btn btn-outline-light"
                        ]
                        { onPress = Nothing
                        , label = Element.text "Remove All Decks"
                        }
                    ]
                , Element.el [ Element.htmlAttribute <| id "decks" ] <|
                    Element.none
                ]
        ]
    }
