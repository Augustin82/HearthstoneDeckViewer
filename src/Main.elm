port module Main exposing (main)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as HA
import Http
import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData)



-- import Url
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
    , cards : WebData Cards
    , decodedDeck : RemoteData String Deck
    }


type alias Cards =
    Dict.Dict Int Card


type alias Card =
    { id : CardId
    , dbfId : Int
    , artist : Maybe String
    , class : String
    , collectible : Bool
    , cost : Maybe Int
    , flavor : Maybe String
    , name : String
    , rarity : String
    , set : String
    , type_ : String
    , text : Maybe String
    , attack : Maybe Int
    , health : Maybe Int
    }



-- type Rarity
--     = Free
--     | Basic
--     | Common
--     | Rare
--     | Epic
--     | Legendary
--
--
-- type HeroClass
--     = Rogue
--     | Priest
--     | Warlock
--     | Shaman
--     | Warrior
--     | DemonHunter
--     | Hunter
--     | Paladin
--     | Mage
--     | Druid


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)


type alias CardId =
    String


cardDecoder : D.Decoder Card
cardDecoder =
    D.succeed Card
        |> andMap (D.field "id" D.string)
        |> andMap (D.field "dbfId" D.int)
        |> andMap (optional "artist" D.string)
        |> andMap (D.field "cardClass" D.string)
        |> andMap (D.field "collectible" D.bool)
        |> andMap (optional "cost" D.int)
        |> andMap (optional "flavor" D.string)
        |> andMap (D.field "name" D.string)
        |> andMap (D.field "rarity" D.string)
        |> andMap (D.field "set" D.string)
        |> andMap (D.field "type" D.string)
        |> andMap (optional "text" D.string)
        |> andMap (optional "attack" D.int)
        |> andMap (optional "health" D.int)


optional : String -> D.Decoder a -> D.Decoder (Maybe a)
optional field dec =
    D.oneOf
        [ D.field field <| D.nullable dec
        , D.succeed Nothing
        ]


cardsDecoder : D.Decoder Cards
cardsDecoder =
    D.list cardDecoder
        |> D.map (List.map (\card -> ( card.dbfId, card )) >> Dict.fromList)


type alias Deck =
    { cards : List ( Int, Int )
    , format : Int
    , heroes : List Int
    }


deckDecoder : D.Decoder Deck
deckDecoder =
    D.succeed Deck
        |> andMap (D.field "cards" (D.list deckCardsDecoder))
        |> andMap (D.field "format" D.int)
        |> andMap (D.field "heroes" (D.list D.int))


deckCardsDecoder : D.Decoder ( Int, Int )
deckCardsDecoder =
    D.list D.int
        |> D.andThen
            (\list ->
                case list of
                    id :: qty :: [] ->
                        case qty of
                            1 ->
                                D.succeed ( id, 1 )

                            2 ->
                                D.succeed ( id, 2 )

                            _ ->
                                D.fail "Quantity must be 1 or 2"

                    _ ->
                        D.fail "Expecting exactly two elements in array"
            )


type Msg
    = UpdateInput String
    | GotCards (WebData Cards)
    | AddDecks
    | DecodedDeck (Result D.Error Deck)
    | NoOp


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
    deckDecoded (D.decodeValue deckDecoder >> DecodedDeck)


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { pasted = ""
      , cards = NotAsked
      , decodedDeck = NotAsked
      }
    , fetchCards
    )


hearthstoneAPICardsURl : String
hearthstoneAPICardsURl =
    "https://api.hearthstonejson.com/v1/latest/enUS/cards.collectible.json"


fetchCards : Cmd Msg
fetchCards =
    Http.get
        { url = hearthstoneAPICardsURl
        , expect = Http.expectJson (RemoteData.fromResult >> GotCards) cardsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInput string ->
            ( { model | pasted = string }, Cmd.none )

        AddDecks ->
            ( model, decodeDeck model.pasted )

        DecodedDeck deck ->
            ( { model
                | decodedDeck =
                    deck
                        |> RemoteData.fromResult
                        |> RemoteData.mapError (\_ -> "Decoding error")
              }
            , Cmd.none
            )

        GotCards result ->
            ( { model | cards = result }, Cmd.none )


view : Model -> Browser.Document Msg
view { pasted, cards, decodedDeck } =
    { title = "Elm version of HS Deck Viewer"
    , body =
        [ layout
            [ height fill
            , width fill
            , Background.color <| rgb255 22 26 58
            ]
          <|
            column [ height fill, width fill ] <|
                [ el
                    [ htmlAttribute <| HA.id "header-section"
                    , width fill
                    , htmlAttribute <|
                        if RemoteData.isSuccess decodedDeck then
                            HA.class ""

                        else
                            HA.style "min-height" "100vh"
                    ]
                  <|
                    el
                        [ height fill
                        , width fill
                        , padding 50
                        ]
                    <|
                        column
                            [ centerY, width fill, spacing 10 ]
                            [ el
                                [ Region.heading 1
                                , padding 50
                                , Font.color <| rgb255 255 255 255
                                , Font.size 40
                                , centerX
                                ]
                              <|
                                text "Hearthstone Deck Viewer"
                            , row [ width fill, Font.size 16 ]
                                [ el [ width <| fillPortion 1 ] <| none
                                , column [ width <| fillPortion 8, htmlAttribute <| HA.style "max-width" "800px", spacing 10 ]
                                    [ row
                                        [ width fill

                                        -- , Font.color <| rgb255 0xF8 0xF9 0xFA
                                        , Font.color <| rgb255 0x49 0x50 0x57
                                        ]
                                        [ Input.text
                                            [ htmlAttribute <| HA.id "deckstring"
                                            , htmlAttribute <| HA.name "deckstring"
                                            , height fill
                                            , width fill
                                            , Border.roundEach <| { topLeft = 5, topRight = 0, bottomLeft = 5, bottomRight = 0 }
                                            , Input.focusedOnLoad
                                            ]
                                            { placeholder =
                                                Just <|
                                                    Input.placeholder [] <|
                                                        text "Input deck code(s) here"
                                            , label = Input.labelHidden ""
                                            , onChange = UpdateInput
                                            , text = pasted
                                            }
                                        , Input.button
                                            [ htmlAttribute <| HA.id "addButton"
                                            , Font.color <| rgb255 255 255 255
                                            , Border.widthEach { top = 1, right = 1, bottom = 1, left = 0 }
                                            , Border.color <| rgb255 255 255 255
                                            , height fill
                                            , paddingXY 12 6
                                            , Border.roundEach <| { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 }
                                            ]
                                            { onPress = Just AddDecks
                                            , label = text "Add Deck(s)"
                                            }
                                        ]
                                    , el [ Font.size 13, Font.color <| rgb255 0x6C 0x75 0x7D, centerX ] <|
                                        text "Separate multiple deck codes with whitespace or commas. Individual deck strings copied from the game client are also supported."
                                    , if not <| RemoteData.isSuccess decodedDeck then
                                        none

                                      else
                                        row
                                            [ htmlAttribute <| HA.id "shortURLForm"
                                            , centerX
                                            , padding 5
                                            ]
                                            [ Input.button
                                                [ htmlAttribute <| HA.id "urlButton"
                                                , Font.color <| rgb255 255 255 255
                                                , Border.widthEach { top = 1, right = 0, bottom = 1, left = 1 }
                                                , Border.color <| rgb255 255 255 255
                                                , height fill
                                                , paddingXY 12 6
                                                , Border.roundEach <| { topLeft = 5, topRight = 0, bottomLeft = 5, bottomRight = 0 }
                                                ]
                                                { onPress = Nothing
                                                , label = text "Generate Short URL"
                                                }
                                            , Input.text
                                                [ htmlAttribute <|
                                                    HA.id "urlInput"
                                                , htmlAttribute <| HA.name "url"
                                                , Border.rounded 0
                                                ]
                                                { onChange = always NoOp
                                                , placeholder = Nothing
                                                , label = Input.labelHidden ""
                                                , text = ""
                                                }
                                            , Input.button
                                                [ htmlAttribute <| HA.id "copyButton"
                                                , Font.color <| rgb255 255 255 255
                                                , Border.widthEach { top = 1, right = 1, bottom = 1, left = 0 }
                                                , Border.color <| rgb255 255 255 255
                                                , height fill
                                                , paddingXY 12 6
                                                , Border.roundEach <| { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 }
                                                ]
                                                { onPress = Nothing
                                                , label = image [ htmlAttribute <| HA.class "clippy", width <| px 13 ] { src = "images/clippy.svg", description = "Copy to clipboard" }
                                                }
                                            ]
                                    , if not <| RemoteData.isSuccess decodedDeck then
                                        none

                                      else
                                        Input.button
                                            [ htmlAttribute <| HA.id "removeButton"
                                            , Font.color <| rgb255 255 255 255
                                            , Border.width 1
                                            , Border.color <| rgb255 255 255 255
                                            , height <| px 42
                                            , paddingXY 12 6
                                            , Border.rounded 5
                                            , centerX
                                            ]
                                            { onPress = Nothing
                                            , label = text "Remove All Decks"
                                            }
                                    ]
                                , el [ width <| fillPortion 1 ] <| none
                                ]
                            ]
                , el
                    [ htmlAttribute <| HA.id "decks"
                    , Background.color <| rgb255 255 255 255
                    , width fill
                    , height fill
                    ]
                  <|
                    el [ centerX ] <|
                        viewDeck cards decodedDeck
                ]
        ]
    }


viewDeck : WebData Cards -> RemoteData String Deck -> Element msg
viewDeck cards deck =
    case deck of
        NotAsked ->
            none

        Loading ->
            none

        Failure err ->
            el [] <| text err

        Success d ->
            column [] <|
                List.map
                    (\( maybeCard, qty ) ->
                        case maybeCard of
                            Nothing ->
                                text "?"

                            Just { name } ->
                                row [] [ text <| String.fromInt qty, text " x ", text name ]
                    )
                <|
                    List.map
                        (Tuple.mapFirst <| \dbfId -> Dict.get dbfId (RemoteData.withDefault Dict.empty cards))
                    <|
                        d.cards


port decodeDeck : String -> Cmd msg


port deckDecoded : (D.Value -> msg) -> Sub msg
