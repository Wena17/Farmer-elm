module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


view model =
    div [ class "container" ]
        [ button [ class "btn btn-primary me-5", type_ "button", onClick Decrement ] [ text "-" ]
        , span [] [ text (String.fromInt model) ]
        , button [ class "btn btn-primary ms-5", type_ "button", onClick Increment ] [ text "+" ]
        ]


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1
