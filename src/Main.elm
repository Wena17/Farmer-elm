module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, attribute, class, id, src, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { message : Maybe String, user : User, products : Maybe (List Product) }


type User
    = Guest


type alias Product =
    { id : Int
    , imgUrl : String
    , name : String
    , prodType : String

    --TODO harvest and expiration dates
    , quantity : Int
    , price : Int
    , unit : String

    --TODO seller
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { message = Just "Loading", user = Guest, products = Just [ exampleProduct, exampleProduct2 ] }
    , Http.get { url = "http://localhost:3000/products", expect = Http.expectJson GotProducts productsDecoder }
    )


exampleProduct =
    { id = 1
    , imgUrl = "/assets/strawberry.jpg"
    , name = "Strawberry"
    , prodType = "fruit"
    , quantity = 7
    , price = 350
    , unit = "kg"
    }


exampleProduct2 =
    { id = 1
    , imgUrl = "/assets/broccoli.jpg"
    , name = "Broccoli"
    , prodType = "vegetable"
    , quantity = 3
    , price = 150
    , unit = "kg"
    }


type Msg
    = LogIn String
    | GotProducts (Result Http.Error (List Product))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ navbar model
        , div [ class "container" ]
            [ case model.message of
                Nothing ->
                    text ""

                Just m ->
                    p [ class "alert alert-primary" ] [ text m ]
            , case model.products of
                Nothing ->
                    p [] [ text "Products not yet loaded." ]

                Just prodList ->
                    if List.length prodList > 0 then
                        div [ class "row gap-5" ] (List.map productToCard prodList)

                    else
                        p [] [ text "No products currently available." ]
            ]
        ]


productToCard : Product -> Html Msg
productToCard product =
    div [ class "card col-lg-4" ]
        [ img [ src product.imgUrl, alt "An image of the product", class "card-img-top pt-3" ] []
        , div [ class "card-body" ]
            [ h5 [] [ text product.name ]
            , p [ class "card-text" ] [ text ("Available: " ++ String.fromInt product.quantity) ]
            , a [ class "btn btn-primary btn-sm" ] [ text "Add to cart" ]
            ]
        ]


navbar model =
    nav [ class "navbar navbar-expand-lg navbar-light bg-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand" ] [ text "Farmer's Coop" ]
            , button
                [ class "navbar-toggler"
                , type_ "button"
                , attribute "data-bs-toggle" "collapse"
                , attribute "data-bs-target" "#navbarSupportedContent"
                ]
                [ span [ class "navbar-toggler-icon" ] [] ]
            , div [ class "collapse navbar-collapse", id "navbarSupportedContent" ]
                [ ul [ class "navbar-nav me-auto mb-2 mb-lg-0" ]
                    [ li [ class "nav-item" ] [ a [ class "nav-link active" ] [ text "Home" ] ]
                    ]
                ]
            , navUser model
            ]
        ]


navUser model =
    case model.user of
        Guest ->
            -- TODO Implement proper login
            a [ class "d-flex btn btn-primary btn-sm", onClick (LogIn "Wena") ] [ text "Log in" ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        m =
            { model | message = Nothing }
    in
    case msg of
        LogIn userName ->
            ( m, Cmd.none )

        GotProducts (Ok prodList) ->
            ( { m | products = Just prodList }, Cmd.none )

        GotProducts (Err e) ->
            ( { m | message = Just (httpErrorToString e) }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        Http.BadUrl s ->
            "Bad URL: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus s ->
            "Bad return code: " ++ String.fromInt s

        Http.BadBody s ->
            "Could not parse the response: " ++ s



-- JSON Decode


productsDecoder : Decoder (List Product)
productsDecoder =
    D.list productDecoder


productDecoder : Decoder Product
productDecoder =
    D.map7 Product
        (D.field "id" D.int)
        (D.field "imgUrl" D.string)
        (D.field "name" D.string)
        (D.field "prodType" D.string)
        (D.field "quantity" D.int)
        (D.field "price" D.int)
        (D.field "unit" D.string)
