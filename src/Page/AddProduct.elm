module Page.AddProduct exposing (Model, Msg, init, update, view)

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
    { name : String
    , description : String
    , prodType : String
    , qty : Float
    , price : Float
    , unit : String
    , key : Nav.Key
    , note : Maybe String
    }


type Msg
    = SubmittedForm
    | ProdNameChanged String
    | ProdDescFieldChanged String
    | ProdTypeChanged String
    | QtyChanged String
    | UnitChanged String
    | PriceChanged String


init : Nav.Key -> Model
init key =
    { key = key
    , name = ""
    , description = ""
    , prodType = ""
    , qty = 0
    , price = 0
    , unit = ""
    , note = Nothing
    }


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
                [ label [ for "prodNameField", class "form-label" ] [ text "Name:" ]
                , input [ class "form-control", id "prodNameField", placeholder "Blueberry", value model.name, onInput ProdNameChanged ] []
                ]
            , div [ class "mb-3" ]
                [ label [ for "prodDescField", class "form-label" ] [ text "Description:" ]
                , input [ type_ "text", class "form-control", id "prodDescField", placeholder "Enter a description here", value model.description, onInput ProdDescFieldChanged ] []
                ]
            , div [ class "mb-3" ]
                [ label [ for "prodTypeField", class "form-label" ] [ text "Type:" ]
                , input [ class "form-control", id "prodTypeField", placeholder "Fruit", value model.prodType, onInput ProdTypeChanged ] []
                ]
            , div [ class "mb-3" ]
                [ label [ for "qtyField", class "form-label" ] [ text "Quantity:" ]
                , input [ type_ "number", class "form-control", id "qtyField", placeholder "20", value (String.fromFloat model.qty), onInput QtyChanged ] []
                ]
            , div [ class "mb-3" ]
                [ label [ for "unitField", class "form-label" ] [ text "Unit:" ]
                , input [ class "form-control", id "unitField", placeholder "kg", value model.unit, onInput UnitChanged ] []
                ]
            , div [ class "mb-3" ]
                [ label [ for "priceField", class "form-label" ] [ text "Price:" ]
                , input [ type_ "number", class "form-control", id "priceField", placeholder "10.99", value (String.fromFloat model.price), onInput PriceChanged ] []
                ]
            , input [ type_ "submit", class "btn btn-primary" ] [ text "Save" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            ( model, Cmd.none )

        ProdNameChanged s ->
            ( { model | name = s }, Cmd.none )

        ProdDescFieldChanged s ->
            ( { model | description = s }, Cmd.none )

        ProdTypeChanged s ->
            ( { model | prodType = s }, Cmd.none )

        QtyChanged s ->
            ( { model | qty = String.toFloat s |> Maybe.withDefault model.qty }, Cmd.none )

        UnitChanged s ->
            ( { model | unit = s }, Cmd.none )

        PriceChanged s ->
            ( { model | price = String.toFloat s |> Maybe.withDefault model.price }, Cmd.none )
