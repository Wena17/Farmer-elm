module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (alt, attribute, class, id, src, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Page.Login
import Url exposing (Url)
import User exposing (User(..))


main =
    Browser.application { init = init, update = update, view = view, subscriptions = subscriptions, onUrlRequest = ClickedLink, onUrlChange = ChangedUrl }


type alias Model =
    { page : Page, user : User, products : Maybe (List Product), note : Maybe String, navKey : Nav.Key }


type Page
    = Home
    | Login Page.Login.Model


type Msg
    = GotProducts (Result Http.Error (List Product))
    | ChangePage Page
    | GotLoginMsg Page.Login.Msg
    | ClickedLink UrlRequest
    | ChangedUrl Url


type alias Product =
    { id : Int
    , imgUrl : String
    , name : String
    , prodType : String

    --TODO harvest and expiration dates
    , quantity : Float
    , price : Float
    , unit : String

    --TODO seller
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { page = Home, user = Guest, products = Nothing, note = Nothing, navKey = key }
    , Http.get { url = "http://localhost:3000/products.json", expect = Http.expectJson GotProducts productsDecoder }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Farmer Cooperative"
    , body =
        [ div []
            [ navbar model
            , div [ class "container" ]
                [ case model.note of
                    Just s ->
                        p [] [ text s ]

                    Nothing ->
                        text ""
                , case model.page of
                    Home ->
                        case model.products of
                            Nothing ->
                                p [] [ text "Products not yet loaded." ]

                            Just prodList ->
                                if List.length prodList > 0 then
                                    div [ class "row gap-5" ] (List.map productToCard prodList)

                                else
                                    p [] [ text "No products available." ]

                    Login loginModel ->
                        Html.map GotLoginMsg (Page.Login.view loginModel)
                ]
            ]
        ]
    }


productToCard : Product -> Html Msg
productToCard product =
    div [ class "card col-lg-4" ]
        [ img [ src product.imgUrl, alt "An image of the product", class "card-img-top pt-3" ] []
        , div [ class "card-body" ]
            [ h5 [] [ text product.name ]
            , p [ class "card-text" ] [ text ("Available: " ++ String.fromFloat product.quantity) ]
            , p [ class "card-text" ] [ text ("Price: " ++ String.fromFloat product.price) ]
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
                    -- TODO Disable page link of current page
                    [ li [ class "nav-item" ] [ a [ class "nav-link active", onClick (ChangePage Home) ] [ text "Home" ] ]
                    ]
                ]
            , navUser model
            ]
        ]


navUser model =
    case model.user of
        Guest ->
            -- TODO Implement proper login
            a [ class "d-flex btn btn-primary btn-sm", onClick (ChangePage (Login Page.Login.init)) ] [ text "Log in" ]

        Member token ->
            -- TODO Implement proper logout
            a [ class "d-flex btn btn-primary btn-sm" ] [ text "Log out" ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            { model | note = Nothing }
    in
    case ( msg, model.page ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    -- TODO Implement page change
                    ( newModel, Cmd.none )

                Browser.External href ->
                    ( newModel, Nav.load href )

        ( ChangedUrl url, _ ) ->
            -- TODO Implement page change within application
            ( newModel, Cmd.none )

        ( ChangePage newPage, _ ) ->
            ( { newModel | page = newPage }, Cmd.none )

        ( GotProducts (Err e), _ ) ->
            ( { newModel | note = Just (httpErrorToString e) }, Cmd.none )

        ( GotProducts (Ok products), _ ) ->
            ( { newModel | products = Just products }, Cmd.none )

        ( GotLoginMsg loginMsg, Login loginModel ) ->
            let
                ( newLoginModel, loginCmd ) =
                    Page.Login.update loginMsg loginModel
            in
            ( { newModel | page = Login newLoginModel }, Cmd.map GotLoginMsg loginCmd )

        ( _, _ ) ->
            -- Silently ignore messages for the wrong page.
            ( newModel, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        Http.BadUrl s ->
            "Bad URL: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus statusCode ->
            "Bad status code: " ++ String.fromInt statusCode

        Http.BadBody s ->
            "Bad body: " ++ s



-- JSON


productsDecoder : Decoder (List Product)
productsDecoder =
    Decode.list
        (Decode.map7 Product
            (Decode.field "id" Decode.int)
            (Decode.field "imgUrl" Decode.string)
            (Decode.field "name" Decode.string)
            (Decode.field "prodType" Decode.string)
            (Decode.field "quantity" Decode.float)
            (Decode.field "price" Json.Decode.Extra.parseFloat)
            (Decode.field "unit" Decode.string)
        )
