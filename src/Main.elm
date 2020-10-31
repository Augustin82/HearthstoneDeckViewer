port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query


type alias Flags =
    ()


type alias Model =
    { pasted : String
    , cards : WebData Cards
    , decodedDecks : Dict.Dict String (RemoteData String Deck)
    , queuedDecks : Dict.Dict String (RemoteData String Deck)
    , currentUrl : String
    , shortUrl : String
    , key : Navigation.Key
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


deckDecoder : D.Decoder ( String, Deck )
deckDecoder =
    D.succeed Tuple.pair
        |> andMap (D.field "deckstring" D.string)
        |> andMap
            (D.field "deck" <|
                (D.succeed Deck
                    |> andMap (D.field "cards" (D.list deckCardsDecoder))
                    |> andMap (D.field "format" D.int)
                    |> andMap (D.field "heroes" (D.list D.int))
                )
            )


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
    | DecodedDeck String (Result String Deck)
    | ClickedLink Browser.UrlRequest
    | CopyDeckCode String
    | RemoveDeck String
    | RemoveAllDecks
    | GenerateShortUrl
    | GotShortUrl String
    | CopyShortUrl
    | UrlChange Url.Url
    | NoOp


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = UrlChange
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    deckDecoded (D.decodeValue deckDecoder >> Result.map (\( deckstring, deck ) -> DecodedDeck deckstring <| Ok deck) >> Result.withDefault NoOp)


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        deckstringsFromUrl =
            url
                |> Url.Parser.parse (Url.Parser.query parseDeckstrings)
                |> Maybe.withDefault []
    in
    ( { pasted = ""
      , cards = NotAsked
      , decodedDecks = Dict.empty
      , queuedDecks = deckstringsFromUrl |> List.map (\d -> ( d, Loading )) |> Dict.fromList
      , currentUrl = Url.toString url
      , shortUrl = ""
      , key = key
      }
    , fetchCards
    )



-- parseDeckstrings : Url.Parser.Parser (List String)


parseDeckstrings : Url.Parser.Query.Parser (List String)
parseDeckstrings =
    Url.Parser.Query.custom "deckstring" identity


hearthstoneAPICardsURl : String
hearthstoneAPICardsURl =
    "https://api.hearthstonejson.com/v1/latest/enUS/cards.collectible.json"


fetchCards : Cmd Msg
fetchCards =
    Http.get
        { url = hearthstoneAPICardsURl
        , expect = Http.expectJson (RemoteData.fromResult >> GotCards) cardsDecoder
        }


requestDecodedDeck : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
requestDecodedDeck code ( m, c ) =
    ( { m | decodedDecks = m.decodedDecks |> Dict.insert code Loading, shortUrl = "" }
    , Cmd.batch
        [ c
        , decodeDeck code
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange url ->
            ( { model | currentUrl = Url.toString url }, Cmd.none )

        GenerateShortUrl ->
            ( model, getShortUrl <| Debug.log "" <| model.currentUrl )

        GotShortUrl shortUrl ->
            ( { model | shortUrl = shortUrl }, Cmd.none )

        CopyShortUrl ->
            ( model, copyToClipboard model.shortUrl )

        CopyDeckCode deckstring ->
            ( model, copyToClipboard deckstring )

        RemoveDeck deckstring ->
            ( { model | decodedDecks = Dict.remove deckstring model.decodedDecks, shortUrl = "" }, focusDeckInput )

        RemoveAllDecks ->
            ( { model | decodedDecks = Dict.empty, shortUrl = "" }, focusDeckInput )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Navigation.load url
                    )

        UpdateInput string ->
            ( { model | pasted = string }, Cmd.none )

        AddDecks ->
            let
                deckcode =
                    model.pasted
            in
            ( { model
                | pasted = ""
              }
            , Cmd.none
            )
                |> requestDecodedDeck deckcode

        DecodedDeck deckstring deck ->
            let
                newDecodedDecks =
                    model.decodedDecks
                        |> Dict.insert deckstring
                            (deck
                                |> RemoteData.fromResult
                                |> RemoteData.mapError (\_ -> "Decoding error")
                            )
            in
            ( { model
                | decodedDecks = newDecodedDecks
                , shortUrl = ""
              }
            , Navigation.pushUrl model.key <|
                Url.Builder.relative [] <|
                    List.map (Url.Builder.string "deckstring") <|
                        Dict.keys newDecodedDecks
            )

        GotCards result ->
            model.queuedDecks
                |> Dict.foldl (\code _ ( m, c ) -> ( m, c ) |> requestDecodedDeck code) ( { model | cards = result, queuedDecks = Dict.empty }, Cmd.none )


view : Model -> Browser.Document Msg
view { pasted, cards, decodedDecks, shortUrl } =
    { title = "Elm version of HS Deck Viewer"
    , body =
        [ Html.node "style" [] [ Html.text "*:focus { outline: none !important; box-shadow: none !important;  }" ]
        , layout
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
                        if List.any RemoteData.isSuccess <| Dict.values decodedDecks then
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
                                            , htmlAttribute <| onEnter AddDecks
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
                                    , if not <| List.any RemoteData.isSuccess <| Dict.values decodedDecks then
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
                                                { onPress = Just <| GenerateShortUrl
                                                , label = text "Generate Short URL"
                                                }
                                            , Input.text
                                                [ htmlAttribute <| HA.id "urlInput"
                                                , htmlAttribute <| HA.readonly True
                                                , htmlAttribute <| HA.name "url"
                                                , Border.rounded 0
                                                ]
                                                { onChange = always NoOp
                                                , placeholder = Nothing
                                                , label = Input.labelHidden ""
                                                , text = shortUrl
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
                                                { onPress = Just CopyShortUrl
                                                , label = image [ htmlAttribute <| HA.class "clippy", width <| px 13 ] { src = "images/clippy.svg", description = "Copy to clipboard" }
                                                }
                                            ]
                                    , if not <| List.any RemoteData.isSuccess <| Dict.values decodedDecks then
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
                                            { onPress = Just RemoveAllDecks
                                            , label = text "Remove All Decks"
                                            }
                                    ]
                                , el [ width <| fillPortion 1 ] <| none
                                ]
                            ]
                , row
                    [ htmlAttribute <| HA.id "decks"
                    , Background.color <| rgb255 255 255 255
                    , width fill
                    , height fill
                    ]
                  <|
                    List.map (el [ centerX ] << viewDeck cards) <|
                        Dict.toList decodedDecks
                ]
        ]
    }


viewDeck : WebData Cards -> ( String, RemoteData String Deck ) -> Element Msg
viewDeck cards ( deckstring, deck ) =
    case deck of
        NotAsked ->
            none

        Loading ->
            none

        Failure err ->
            el [] <| text err

        Success d ->
            el [ padding 10 ] <|
                column [ spacing 0, Font.size 16, width <| px 240 ]
                    [ deckTitle cards d
                    , deckCards cards d
                    , deckButtons deckstring
                    ]


deckTitle : WebData Cards -> Deck -> Element msg
deckTitle cards deck =
    let
        hero =
            deck.heroes
                |> List.head

        cardClass =
            case hero of
                Just h ->
                    Dict.get h <|
                        RemoteData.withDefault Dict.empty cards

                Nothing ->
                    Nothing
    in
    hero
        |> Maybe.map
            (\int ->
                el
                    [ width fill
                    , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
                    , htmlAttribute <| HA.style "background-image" <| "url(" ++ imageUrlForHero cards int ++ ")"
                    , htmlAttribute <| HA.style "background-position" "right bottom 90px"
                    ]
                <|
                    el [ centerX, centerY, Font.size 20, padding 21, Font.glow (rgb 0 0 0) 1, Font.color <| rgb 1 1 1 ] <|
                        text <|
                            Maybe.withDefault "UNKNOWN" <|
                                Maybe.map .class <|
                                    cardClass
            )
        |> Maybe.withDefault none


deckCards : WebData Cards -> Deck -> Element msg
deckCards cards deck =
    column [ width fill ] <|
        List.map (\( card, qty ) -> viewDeckCard card qty) <|
            List.sortWith (\( c1, _ ) ( c2, _ ) -> manaCostAndThenName c1 c2) <|
                List.filterMap
                    (\( dbfId, qty ) ->
                        cards
                            |> RemoteData.withDefault Dict.empty
                            |> Dict.get dbfId
                            |> Maybe.map (\c -> ( c, qty ))
                    )
                <|
                    deck.cards


manaCostAndThenName : Card -> Card -> Order
manaCostAndThenName c1 c2 =
    let
        cost1 =
            c1.cost |> Maybe.withDefault 0

        cost2 =
            c2.cost |> Maybe.withDefault 0
    in
    if cost1 == cost2 then
        compare c1.name c2.name

    else
        compare cost1 cost2


deckButtons : String -> Element Msg
deckButtons deckstring =
    column
        [ width fill
        , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
        , Border.width 1
        , Border.color <| rgb 0 0 0
        , padding 10
        , spacing 10
        ]
        [ Input.button
            [ htmlAttribute <| HA.id "copyDeckCode"
            , Font.color <| rgb 0 0 0
            , Border.width 1
            , Border.color <| rgb 0 0 0
            , height fill
            , width fill
            , paddingXY 12 6
            , Border.rounded 3
            , Font.center
            , Font.size 14
            ]
            { onPress = Just <| CopyDeckCode deckstring
            , label = text "Copy Deck Code"
            }
        , Input.button
            [ htmlAttribute <| HA.id "removeDeck"
            , Font.color <| rgb 0 0 0
            , Border.width 1
            , Border.color <| rgb 0 0 0
            , height fill
            , width fill
            , paddingXY 12 6
            , Border.rounded 3
            , Font.center
            , Font.size 14
            ]
            { onPress = Just <| RemoveDeck deckstring
            , label = text "Remove Deck"
            }
        ]


imageUrlForId : CardId -> String
imageUrlForId id =
    "/images/tiles/" ++ id ++ ".png"


imageUrlForHero : WebData Cards -> Int -> String
imageUrlForHero cards dbfId =
    let
        id =
            Dict.get dbfId (RemoteData.withDefault Dict.empty cards)
                |> Maybe.map .id
                |> Maybe.withDefault ""
    in
    "/images/heroes/" ++ id ++ ".jpg"


manaCrystal : String
manaCrystal =
    "/images/mana_crystal.png"


viewDeckCard : Card -> Int -> Element msg
viewDeckCard { name, cost, id } qty =
    row
        [ width fill
        , Font.color <| rgb255 255 255 255
        , height <| px 30
        , Background.image <| imageUrlForId id
        ]
        [ el
            [ width <| px 25
            , Background.color <| rgb255 22 26 58
            , htmlAttribute <| HA.style "background-image" <| "url(" ++ manaCrystal ++ ")"
            , htmlAttribute <| HA.style "background-size" "25px 22.5px"
            , htmlAttribute <| HA.style "background-repeat" "no-repeat"
            , htmlAttribute <| HA.style "background-position" "right top 3px"
            , height fill
            ]
          <|
            el [ centerY, centerX, Font.glow (rgb 0 0 0) 1 ] <|
                text <|
                    Maybe.withDefault "" <|
                        Maybe.map String.fromInt <|
                            cost
        , el
            [ width fill
            , height fill
            , htmlAttribute <| HA.style "background" "linear-gradient(45deg, rgba(22,26,58,1) 0%,rgba(15,18,41,1) 30%,rgba(0,0,0,0) 100%)"
            ]
          <|
            el
                [ width fill
                , Font.center
                , centerY
                , Font.glow (rgb 0 0 0) 1
                ]
            <|
                text name
        , el
            [ width <| px 15
            , Background.color <| rgb255 22 26 58
            , height fill
            ]
          <|
            el [ centerY, centerX ] <|
                text <|
                    String.fromInt qty
        ]


onEnter : Msg -> Html.Attribute Msg
onEnter tagger =
    HE.on "keyup"
        (HE.keyCode
            |> D.andThen
                (\key ->
                    if key == 13 then
                        D.succeed tagger

                    else
                        D.fail "Ignoring, not 'Enter'!"
                )
        )


focusDeckInput : Cmd Msg
focusDeckInput =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus "deckstring")


getShortUrl : String -> Cmd Msg
getShortUrl currentUrl =
    Http.post
        { url = "/.netlify/functions/shorturl"
        , body = Http.jsonBody <| Encode.object [ ( "longUrl", Encode.string currentUrl ) ]
        , expect =
            Http.expectJson
                (Result.map GotShortUrl >> Result.withDefault (GotShortUrl "url error"))
                (D.field "shortUrl" D.string)
        }


port decodeDeck : String -> Cmd msg


port copyToClipboard : String -> Cmd msg


port deckDecoded : (D.Value -> msg) -> Sub msg
