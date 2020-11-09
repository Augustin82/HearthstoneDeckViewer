module Json.Decode.Utils exposing (..)

import Json.Decode as Decode


andMap : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
andMap =
    Decode.map2 (|>)


optional : String -> Decode.Decoder a -> Decode.Decoder (Maybe a)
optional field dec =
    Decode.oneOf
        [ Decode.field field <| Decode.nullable dec
        , Decode.succeed Nothing
        ]
