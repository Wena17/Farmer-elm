module Page.Login exposing (Msg, init, view)

import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_)


type alias Model =
    { userName : String, password : String }


type Msg
    = Noop


init : Model
init =
    { userName = "", password = "" }


view : Model -> Html Msg
view model =
    form []
        [ div [ class "mb-3" ]
            [ label [ for "userNameField", class "form-label" ] [ text "User name:" ]
            , input [ class "form-control", id "userNameField", placeholder "Enter your user name" ] []
            ]
        , div [ class "mb-3" ]
            [ label [ for "passwordField", class "form-label" ] [ text "Password:" ]
            , input [ type_ "password", class "form-control", id "passwordField", placeholder "Enter your password" ] []
            ]
        , button [ type_ "submit", class "btn btn-primary" ] [ text "Login" ]
        ]
