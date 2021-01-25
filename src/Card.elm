module Card exposing (..)

import Dict
import Json.Decode as Decode
import Json.Decode.Utils as Pipeline


type alias Cards =
    Dict.Dict Int Card


type alias Card =
    { id : Id
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


type alias Id =
    String


cardDecoder : Decode.Decoder Card
cardDecoder =
    Decode.succeed Card
        |> Pipeline.andMap (Decode.field "id" Decode.string)
        |> Pipeline.andMap (Decode.field "dbfId" Decode.int)
        |> Pipeline.andMap (Pipeline.optional "artist" Decode.string)
        |> Pipeline.andMap (Decode.field "cardClass" Decode.string)
        |> Pipeline.andMap (Decode.field "collectible" Decode.bool)
        |> Pipeline.andMap (Pipeline.optional "cost" Decode.int)
        |> Pipeline.andMap (Pipeline.optional "flavor" Decode.string)
        |> Pipeline.andMap (Decode.field "name" Decode.string)
        |> Pipeline.andMap (Decode.field "rarity" Decode.string)
        |> Pipeline.andMap (Decode.field "set" Decode.string)
        |> Pipeline.andMap (Decode.field "type" Decode.string)
        |> Pipeline.andMap (Pipeline.optional "text" Decode.string)
        |> Pipeline.andMap (Pipeline.optional "attack" Decode.int)
        |> Pipeline.andMap (Pipeline.optional "health" Decode.int)


cardsDecoder : Decode.Decoder Cards
cardsDecoder =
    Decode.list cardDecoder
        |> Decode.map (List.map (\card -> ( card.dbfId, card )) >> Dict.fromList)


type alias Deck =
    { cards : List ( Int, Int )
    , format : Int
    , heroes : List Int
    , title : Maybe String
    }


deckDecoder : Decode.Decoder ( String, Deck )
deckDecoder =
    Decode.succeed (\ds d t -> ( ds, { d | title = t } ))
        |> Pipeline.andMap (Decode.field "deckstring" Decode.string)
        |> Pipeline.andMap
            (Decode.field "deck" <|
                (Decode.succeed Deck
                    |> Pipeline.andMap (Decode.field "cards" (Decode.list deckCardsDecoder))
                    |> Pipeline.andMap (Decode.field "format" Decode.int)
                    |> Pipeline.andMap (Decode.field "heroes" (Decode.list Decode.int))
                    |> Pipeline.andMap (Decode.succeed Nothing)
                )
            )
        |> Pipeline.andMap (Decode.field "title" <| Decode.nullable Decode.string)


deckCardsDecoder : Decode.Decoder ( Int, Int )
deckCardsDecoder =
    Decode.list Decode.int
        |> Decode.andThen
            (\list ->
                case list of
                    id :: qty :: [] ->
                        case qty of
                            1 ->
                                Decode.succeed ( id, 1 )

                            2 ->
                                Decode.succeed ( id, 2 )

                            _ ->
                                Decode.fail "Quantity must be 1 or 2"

                    _ ->
                        Decode.fail "Expecting exactly two elements in array"
            )


type alias Deckstring =
    String
