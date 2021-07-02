module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, attribute, class, id, src, type_)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { user : User, products : Maybe (List Product) }


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


init =
    { user = Guest, products = Just [ exampleProduct, exampleProduct2 ] }


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



-- View


view model =
    div []
        [ navbar model
        , div [ class "container" ]
            [ case model.products of
                Nothing ->
                    p [] [ text "Products not yet loaded." ]

                Just prodList ->
                    if List.length prodList > 0 then
                        div [ class "row gap-5" ] (List.map productToCard prodList)

                    else
                        p [] [ text "No products available." ]
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


update msg model =
    case msg of
        LogIn userName ->
            model
