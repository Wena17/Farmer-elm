module Page.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Dict
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Http.Detailed
import Json.Encode as Encode
import Url.Builder
import User exposing (User(..))


type alias Model =
    { email : String, password : String, key : Nav.Key, user : Maybe User, note : Maybe String }


type Msg
    = EmailChanged String
    | PasswordChanged String
    | SubmittedForm
    | CompletedLogin (Result (Http.Detailed.Error String) ( Http.Metadata, User ))


init : Nav.Key -> Model
init key =
    { email = "", password = "", key = key, user = Nothing, note = Nothing }


view : Model -> Html Msg
view model =
    div []
        [ case model.note of
            Nothing ->
                div [] []

            Just s ->
                div [ class "alert alert-warning" ] [ text s ]
        , Html.form [ onSubmit SubmittedForm ]
            [ div [ class "mb-3" ]
                [ label [ for "emailField", class "form-label" ] [ text "Email:" ]
                , input [ type_ "email", class "form-control", id "emailField", placeholder "juan@example.com", value model.email, onInput EmailChanged ] []
                ]
            , div [ class "mb-3" ]
                [ label [ for "passwordField", class "form-label" ] [ text "Password:" ]
                , input [ type_ "password", class "form-control", id "passwordField", placeholder "***", value model.password, onInput PasswordChanged ] []
                ]
            , input [ type_ "submit", class "btn btn-primary" ] [ text "Login" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            { model | note = Nothing }
    in
    case msg of
        EmailChanged s ->
            ( { model | email = s }, Cmd.none )

        PasswordChanged s ->
            ( { model | password = s }, Cmd.none )

        SubmittedForm ->
            ( { newModel | password = "" }
            , Http.post
                { url = "/users/sign_in"
                , body = Http.jsonBody (modelToJson model)
                , expect = Http.Detailed.expectJson CompletedLogin User.decoder
                }
            )

        CompletedLogin (Ok ( metadata, user )) ->
            case Dict.get "authorization" metadata.headers of
                Nothing ->
                    ( { newModel | note = Just "Authentication failed, server did not let us in." }, Cmd.none )

                Just token ->
                    ( { newModel | user = Just (Member token) }, Nav.pushUrl model.key (Url.Builder.absolute [] []) )

        CompletedLogin (Err err) ->
            let
                n =
                    case err of
                        Http.Detailed.BadUrl _ ->
                            "Client error"

                        Http.Detailed.Timeout ->
                            "Server timeout"

                        Http.Detailed.NetworkError ->
                            "Network error"

                        Http.Detailed.BadStatus meta _ ->
                            if meta.statusCode == 401 then
                                "Authentication failed"

                            else
                                "Bad status code: " ++ String.fromInt meta.statusCode

                        Http.Detailed.BadBody _ _ _ ->
                            "Bad body"
            in
            ( { newModel | note = Just n }, Cmd.none )


modelToJson : Model -> Encode.Value
modelToJson model =
    Encode.object
        [ ( "user"
          , Encode.object
                [ ( "email", Encode.string model.email )
                , ( "password", Encode.string model.password )
                ]
          )
        ]
