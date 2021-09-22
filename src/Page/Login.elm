module Page.Login exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Encode as Encode
import User exposing (User)


type alias Model =
    { email : String, password : String }


type Msg
    = EmailChanged String
    | PasswordChanged String
    | SubmittedForm
    | CompletedLogin (Result Http.Error User)


init : Model
init =
    { email = "", password = "" }


view : Model -> Html Msg
view model =
    Html.form [ onSubmit SubmittedForm ]
        [ div [ class "mb-3" ]
            [ label [ for "emailField", class "form-label" ] [ text "Email:" ]
            , input [ type_ "email", class "form-control", id "emailField", placeholder "juan@example.com", onInput EmailChanged ] []
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
        EmailChanged s ->
            ( { model | email = s }, Cmd.none )

        PasswordChanged s ->
            ( { model | password = s }, Cmd.none )

        SubmittedForm ->
            ( model
            , Http.post
                { url = "http://localhost:3000/users/sign_in"
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
                [ ( "email", Encode.string model.email )
                , ( "password", Encode.string model.password )
                ]
          )
        ]
