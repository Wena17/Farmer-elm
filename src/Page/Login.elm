module Page.Login exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Encode as Encode
import User exposing (User)


type alias Model =
    { name : String, password : String }


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | SubmittedForm
    | CompletedLogin (Result Http.Error User)


init : Model
init =
    { name = "", password = "" }


view : Model -> Html Msg
view model =
    Html.form [ onSubmit SubmittedForm ]
        [ div [ class "mb-3" ]
            [ label [ for "usernameField", class "form-label" ] [ text "Username:" ]
            , input [ type_ "text", class "form-control", id "usernameField", placeholder "username", onInput UsernameChanged ] []
            ]
        , div [ class "mb-3" ]
            [ label [ for "passwordField", class "form-label" ] [ text "Password:" ]
            , input [ type_ "password", class "form-control", id "passwordField", placeholder "***", onInput PasswordChanged ] []
            ]
        , input [ type_ "submit", class "btn btn-primary" ] [ text "Login" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: Implement
    case msg of
        UsernameChanged s ->
            ( { model | name = s }, Cmd.none )

        PasswordChanged s ->
            ( { model | password = s }, Cmd.none )

        SubmittedForm ->
            ( model
            , Http.post
                { url = "http://localhost:3000/users/login"
                , body = Http.jsonBody (modelToJson model)
                , expect = Http.expectJson CompletedLogin User.decoder
                }
            )

        CompletedLogin (Ok user) ->
            -- TODO Implement remembering user and redirect to home page
            ( model, Cmd.none )

        CompletedLogin (Err err) ->
            -- TODO Implement showing error message
            ( model, Cmd.none )


modelToJson : Model -> Encode.Value
modelToJson model =
    Encode.object
        [ ( "user"
          , Encode.object
                [ ( "username", Encode.string model.name )
                , ( "password", Encode.string model.password )
                ]
          )
        ]
