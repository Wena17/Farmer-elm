module User exposing (User(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type User
    = Guest


decoder : Decoder User
decoder =
    -- TODO Implement proper decoder.
    Decode.succeed Guest
