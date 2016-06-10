import Html.App as App
import Html exposing (Html, button, div, text, h1, section, li, ul, table, thead, tbody, tr, th, td, tfoot, strong)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import List exposing (length, map, isEmpty)
import Cart exposing (..)
import Time exposing (..)
import Date.Format exposing (format)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
    (model, Cmd.none)


-- Model
type alias Product = { id : Int, price : Float, title : String }


type alias Model =
  { cart: Cart.Cart Product
  , products : List Product
  , timer: Time
  }


model : Model
model =
  { cart = Cart.cart
  , products =
    [ Product 1 10.0 "Product A"
    , Product 2 20.0 "Product B"
    ]
  , timer = 0
  }

-- Update

type Msg = NoOp | Add Product | Inc Product | Dec Product | Remove Product | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    Add p ->
      ({ model | cart = Cart.add p model.cart model.timer }, Cmd.none)

    Inc p ->
      ({ model | cart = Cart.inc p model.cart }, Cmd.none)

    Dec p ->
      ({ model | cart = Cart.dec p model.cart }, Cmd.none)

    Remove p ->
      ({ model | cart = Cart.remove p model.cart }, Cmd.none)

    Tick time ->
      ({ model | timer = time }, Cmd.none)



-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- View

view : Model -> Html Msg
view model =
  section []
    [ h1 [] [ text "Products" ]
    , ul [] (map (productItem) model.products)
    , h1 [] [ text "Cart" ],
      if isEmpty model.cart
        then (text "Empty Cart")
        else (table []
          [ thead []
            [ tr []
              [ th [] [text "Title"]
              , th [] [text "Date Added"]
              , th [] [text "Price"]
              , th [colspan 2] [text "Quantity"]
              , th [] [text "Subtotal"]
              ]
            ]
          , tbody [] (List.map cartItem model.cart)
          , tfoot []
            [ tr [align "right"]
              [ td [ colspan 5 ]
                [ strong [] [ text "Subtotal: "]]
              , td []
                [ strong [] [ text <| priceFormat <| Cart.subtotal .price model.cart ] ]
              ]
            ]
          ])
    , div [] (if isEmpty model.cart then [] else [ button [] [ text "Checkout" ]])
    ]


priceFormat : Float -> String
priceFormat price =
  "$" ++ toString price


productItem : Product -> Html Msg
productItem product =
  li
    []
    [ text <| product.title ++ " -- " ++ (priceFormat product.price) ++ " "
    , button [ onClick <| Add product ] [ text "Add to Cart" ]
    ]

cartItem : Cart.Item Product -> Html Msg
cartItem item =
  tr []
    [ td [] [ text item.product.title ]
    , td [] [ text <| format "%e %b, %Y %H:%M" item.date_added ]
    , td [] [ text <| priceFormat item.product.price ]
    , td [] [ text <| toString item.qty ]
    , td []
      [ button [ onClick <| Inc item.product ] [ text "+" ]
      , button [ onClick <| Dec item.product ] [ text "-" ]
      , button [ onClick <| Remove item.product ] [ text "x" ]
      ]
    , td [ align "right" ] [ text <| priceFormat <| Cart.itemSubtotal .price item ]
  ]
