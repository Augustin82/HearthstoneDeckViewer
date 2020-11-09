port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Button
import Card
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Decode
import Json.Decode.Utils as Pipeline
import Json.Encode as Encode
import OrderedDict exposing (OrderedDict)
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query
import Utils


type alias Flags =
    ()


type alias Model =
    { pasted : String
    , cards : WebData Card.Cards
    , decodedDecks : OrderedDict String (RemoteData String Card.Deck)
    , queuedDecks : OrderedDict String (RemoteData String Card.Deck)
    , currentUrl : String
    , shortUrl : String
    , tooltip : Maybe Tooltip
    , key : Navigation.Key
    }


type alias HttpError =
    String


type alias Tooltip =
    ( Card.Deckstring, Card.Id )


type Msg
    = UpdateInput String
    | GotCards (WebData Card.Cards)
    | AddDecks
    | DecodedDeck Card.Deckstring (Result HttpError Card.Deck)
    | ClickedLink Browser.UrlRequest
    | CopyDeckCode Card.Deckstring
    | RemoveDeck Card.Deckstring
    | RemoveAllDecks
    | GenerateShortUrl
    | GotShortUrl String
    | CopyShortUrl
    | UrlChange Url.Url
    | ShowTooltip Tooltip
    | HideTooltip
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
    deckDecoded (Decode.decodeValue Card.deckDecoder >> Result.map (\( deckstring, deck ) -> DecodedDeck deckstring <| Ok deck) >> Result.withDefault NoOp)


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
      , decodedDecks = OrderedDict.empty
      , queuedDecks = deckstringsFromUrl |> List.map (\d -> ( d, Loading )) |> Dict.fromList |> OrderedDict deckstringsFromUrl
      , currentUrl = Url.toString url
      , shortUrl = ""
      , tooltip = Nothing
      , key = key
      }
    , fetchCards
    )


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
        , expect = Http.expectJson (RemoteData.fromResult >> GotCards) Card.cardsDecoder
        }


requestDecodedDeck : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
requestDecodedDeck code ( m, c ) =
    ( { m | decodedDecks = m.decodedDecks |> OrderedDict.insert code Loading, shortUrl = "" }
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

        ShowTooltip tooltip ->
            ( { model | tooltip = Just tooltip }, fixTooltipPlacement (Tuple.first tooltip ++ Tuple.second tooltip) )

        HideTooltip ->
            ( { model | tooltip = Nothing }, Cmd.none )

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
            ( { model | decodedDecks = OrderedDict.remove deckstring model.decodedDecks, shortUrl = "" }, focusDeckInput )

        RemoveAllDecks ->
            ( { model | decodedDecks = OrderedDict.empty, shortUrl = "" }, focusDeckInput )

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
                        |> OrderedDict.insert deckstring
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
                        .order <|
                            newDecodedDecks
            )

        GotCards result ->
            model.queuedDecks
                |> .dict
                |> Dict.foldl (\code _ ( m, c ) -> ( m, c ) |> requestDecodedDeck code) ( { model | cards = result, queuedDecks = OrderedDict.empty }, Cmd.none )


view : Model -> Browser.Document Msg
view { pasted, cards, decodedDecks, tooltip, shortUrl } =
    { title = "Elm version of HS Deck Viewer"
    , body =
        [ layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
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
                        if List.any RemoteData.isSuccess <| OrderedDict.orderedValues decodedDecks then
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
                                            , htmlAttribute <| Utils.onEnter AddDecks
                                            ]
                                            { placeholder =
                                                Just <|
                                                    Input.placeholder [] <|
                                                        text "Input deck code(s) here"
                                            , label = Input.labelHidden ""
                                            , onChange = UpdateInput
                                            , text = pasted
                                            }
                                        , Button.primaryButton
                                            { onPress = Just AddDecks
                                            , label = text "Add Deck(s)"
                                            }
                                            |> Button.withAttrs [ htmlAttribute <| HA.id "addButton", height fill ]
                                            |> Button.withRounded (Border.roundEach <| { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 })
                                            |> Button.withBorderWidth (Border.widthEach { top = 1, right = 1, bottom = 1, left = 0 })
                                            |> Button.viewButton
                                        ]
                                    , el [ Font.size 13, Font.color <| rgb255 0x6C 0x75 0x7D, centerX ] <|
                                        text "Separate multiple deck codes with whitespace or commas. Individual deck strings copied from the game client are also supported."
                                    , if not <| List.any RemoteData.isSuccess <| OrderedDict.orderedValues decodedDecks then
                                        none

                                      else
                                        row
                                            [ htmlAttribute <| HA.id "shortURLForm"
                                            , centerX
                                            , padding 5
                                            ]
                                            [ Button.primaryButton
                                                { onPress = Just <| GenerateShortUrl
                                                , label = text "Generate Short URL"
                                                }
                                                |> Button.withAttrs [ htmlAttribute <| HA.id "urlButton", height fill ]
                                                |> Button.withRounded (Border.roundEach <| { topLeft = 5, topRight = 0, bottomLeft = 5, bottomRight = 0 })
                                                |> Button.withBorderWidth (Border.widthEach { top = 1, right = 0, bottom = 1, left = 1 })
                                                |> Button.viewButton
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
                                            , Button.primaryButton
                                                { onPress = Just CopyShortUrl
                                                , label = image [ htmlAttribute <| HA.class "clippy", width <| px 13 ] { src = "images/clippy.svg", description = "Copy to clipboard" }
                                                }
                                                |> Button.withAttrs [ htmlAttribute <| HA.id "copyButton", height fill ]
                                                |> Button.withRounded (Border.roundEach <| { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 })
                                                |> Button.withBorderWidth (Border.widthEach { top = 1, right = 1, bottom = 1, left = 0 })
                                                |> Button.viewButton
                                            ]
                                    , if not <| List.any RemoteData.isSuccess <| OrderedDict.orderedValues decodedDecks then
                                        none

                                      else
                                        Button.primaryButton
                                            { onPress = Just RemoveAllDecks
                                            , label = text "Remove All Decks"
                                            }
                                            |> Button.withAttrs
                                                [ htmlAttribute <| HA.id "removeButton"
                                                , height <| px 42
                                                , centerX
                                                ]
                                            |> Button.viewButton
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
                    List.map (el [ centerX ] << viewDeck cards tooltip) <|
                        (\decks ->
                            decks.order
                                |> List.filterMap
                                    (\key ->
                                        decks
                                            |> .dict
                                            |> Dict.get key
                                            |> Maybe.map (Tuple.pair key)
                                    )
                        )
                        <|
                            decodedDecks
                ]
        ]
    }


viewDeck : WebData Card.Cards -> Maybe Tooltip -> ( String, RemoteData String Card.Deck ) -> Element Msg
viewDeck cards tooltip ( deckstring, deck ) =
    case deck of
        NotAsked ->
            none

        Loading ->
            none

        Failure err ->
            el [] <| text err

        Success d ->
            el
                [ padding 10
                , htmlAttribute <|
                    HA.style "z-index" <|
                        if tooltip |> Maybe.map Tuple.first |> Maybe.map ((==) deckstring) |> Maybe.withDefault False then
                            "1000"

                        else
                            "900"
                ]
            <|
                column [ spacing 0, Font.size 16, width <| px 240 ]
                    [ deckTitle cards d
                    , deckCards cards tooltip deckstring d
                    , deckButtons deckstring
                    ]


deckTitle : WebData Card.Cards -> Card.Deck -> Element msg
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


deckCards : WebData Card.Cards -> Maybe Tooltip -> Card.Deckstring -> Card.Deck -> Element Msg
deckCards cards tooltip deckstring deck =
    column [ width fill ] <|
        List.map (\( card, qty ) -> viewDeckCard tooltip deckstring card qty) <|
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


manaCostAndThenName : Card.Card -> Card.Card -> Order
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
        [ Button.secondaryButton
            { onPress = Just <| CopyDeckCode deckstring
            , label = text "Copy Deck Code"
            }
            |> Button.withAttrs
                [ htmlAttribute <| HA.id "copyDeckCode"
                , height <| px 42
                , width fill
                , centerX
                , Font.center
                ]
            |> Button.withFontSize (Font.size 14)
            |> Button.viewButton
        , Button.secondaryButton
            { onPress = Just <| RemoveDeck deckstring
            , label = text "Remove Deck"
            }
            |> Button.withAttrs
                [ htmlAttribute <| HA.id "removeDeck"
                , height <| px 42
                , width fill
                , centerX
                , Font.center
                ]
            |> Button.withFontSize (Font.size 14)
            |> Button.viewButton
        ]


tileUrlForId : Card.Id -> String
tileUrlForId id =
    "/images/tiles/" ++ id ++ ".png"


imageUrlForId : Card.Id -> String
imageUrlForId id =
    "/images/cards/" ++ id ++ ".png"


imageUrlForHero : WebData Card.Cards -> Int -> String
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


viewDeckCard : Maybe Tooltip -> Card.Deckstring -> Card.Card -> Int -> Element Msg
viewDeckCard tooltip deckstring { name, cost, id } qty =
    row
        [ width fill
        , Font.color <| rgb255 255 255 255
        , height <| px 30
        , Background.image <| tileUrlForId id
        , Events.onMouseEnter <| ShowTooltip <| ( deckstring, id )

        -- , Events.onMouseLeave <| HideTooltip
        , onRight <|
            case tooltip of
                Nothing ->
                    none

                Just ( d, cId ) ->
                    if d == deckstring && cId == id then
                        row
                            [ htmlAttribute <| HA.style "z-index" "1100"
                            , htmlAttribute <| HA.id <| deckstring ++ id
                            , moveUp 10
                            ]
                            [ el
                                [ width <| px 0
                                , height <| px 0
                                , alignTop
                                , Background.color <| rgba 1 1 1 0
                                , htmlAttribute <| HA.style "border-top" "10px solid transparent"
                                , htmlAttribute <| HA.style "border-bottom" "10px solid transparent"
                                , htmlAttribute <| HA.style "border-right" "10px solid #161A3A"
                                , moveDown 15
                                ]
                              <|
                                text ""
                            , el
                                [ Background.color <| rgb255 22 26 58
                                , Border.rounded 15
                                , height fill
                                , width <| px 300
                                , height <| px 450
                                ]
                              <|
                                image [ centerX, centerY ]
                                    { src = imageUrlForId id
                                    , description = name
                                    }
                            ]

                    else
                        none
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
                (Decode.field "shortUrl" Decode.string)
        }


port decodeDeck : String -> Cmd msg


port copyToClipboard : String -> Cmd msg


port fixTooltipPlacement : String -> Cmd msg


port deckDecoded : (Decode.Value -> msg) -> Sub msg
