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
    { email : String, password : String, key : Nav.Key }


type Msg
    = EmailChanged String
    | PasswordChanged String
    | SubmittedForm
    | CompletedLogin (Result (Http.Detailed.Error String) ( Http.Metadata, User ))


init : Nav.Key -> Model
init key =
    { email = "", password = "", key = key }


view : Model -> Html Msg
view model =
    Html.form [ onSubmit SubmittedForm ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: Implement
    case msg of
        EmailChanged s ->
            ( { model | email = s }, Cmd.none )

        PasswordChanged s ->
            ( { model | password = s }, Cmd.none )

        SubmittedForm ->
            ( { model | password = "" }
            , Http.post
                { url = "http://localhost:3000/users/sign_in.json"
                , body = Http.jsonBody (modelToJson model)
                , expect = Http.Detailed.expectJson CompletedLogin User.decoder
                }
            )

        CompletedLogin (Ok ( metadata, user )) ->
            case Dict.get "authorization" metadata.headers of
                Nothing ->
                    -- TODO Implement notification that login failed
                    ( model, Cmd.none )

                Just token ->
                    let
                        authenticatedUser =
                            Debug.log "Member: " (Member token)
                    in
                    -- TODO Implement remembering user and redirect to home page
                    ( model, Nav.pushUrl model.key (Url.Builder.absolute [] []) )

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
