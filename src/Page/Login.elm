module Page.Login exposing (Msg, init, update, view)

import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Encode as Encode
import User exposing (User(..))


type alias Model =
    { userName : String, password : String }


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | LoginSubmitted
    | GotResponse (Result Http.Error User)


init : Model
init =
    { userName = "", password = "" }


view : Model -> Html Msg
view model =
    form [ onSubmit LoginSubmitted ]
        [ div [ class "mb-3" ]
            [ label [ for "userNameField", class "form-label" ] [ text "User name:" ]
            , input [ class "form-control", id "userNameField", placeholder "Enter your user name", onInput UsernameChanged ] []
            ]
        , div [ class "mb-3" ]
            [ label [ for "passwordField", class "form-label" ] [ text "Password:" ]
            , input [ type_ "password", class "form-control", id "passwordField", placeholder "Enter your password", onInput PasswordChanged ] []
            ]
        , button [ type_ "submit", class "btn btn-primary" ] [ text "Login" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameChanged s ->
            ( { model | userName = s }, Cmd.none )

        PasswordChanged s ->
            ( { model | password = s }, Cmd.none )

        LoginSubmitted ->
            ( model
            , Http.post
                { url = "http://localhost:3000/user/login"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "user"
                              , Encode.object
                                    [ ( "userName", Encode.string model.userName )
                                    , ( "password", Encode.string model.password )
                                    ]
                              )
                            ]
                        )
                , expect = Http.expectJson GotResponse User.decoder
                }
            )

        GotResponse (Ok user) ->
            -- TODO Remember user and change to home page
            ( model, Cmd.none )

        GotResponse (Err e) ->
            -- TODO Display error message
            ( model, Cmd.none )
