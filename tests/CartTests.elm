import String
import Html exposing (..)
import ElmTest exposing (..)
import Cart exposing (..)
import List exposing (..)
import Date exposing (Date)


type alias Product = { id : Int, price : Float, title : String }

setUp : { cart : Cart Product, productA : Product, productB : Product, productC : Product }
setUp =
  { cart = cart
  , productA = Product 1 10.0 "A"
  , productB = Product 2 20.0 "B"
  , productC = Product 3 30.0 "C"
  }


testCartItem : List Test
testCartItem =
  let
    data = setUp
    cart1 = add data.productA data.cart 12345
    item1 = head cart1
  in
    append
      [ test "Check cart not empty" <|
          assert (not (isEmpty cart1)) ]
      (case item1 of
          Nothing ->
            [ test "Check item exist" (fail "It cannot be Nothig") ]

          Just item ->
            [ test "Check item subtotal" <|
                assertEqual data.productA.price (itemSubtotal .price item)
            , test "Check item product" <|
                assertEqual data.productA item.product
            , test "Check date added" <|
                assertEqual (Date.fromTime 12345) item.date_added
            , test "Check qty 1" <|
                assertEqual 1 item.qty
            ])


testAdd: List Test
testAdd =
  let
    data = setUp
    cart1 = add data.productA data.cart 12345
    cart2 = add data.productB cart1 23456
    cart3 = add data.productC cart2 34567
  in
    [ test "Check add product A subtotal" <|
        assertEqual data.productA.price (subtotal .price cart1)
    , test "Check add product A+B subtotal" <|
        assertEqual (data.productA.price + data.productB.price) (subtotal .price cart2)
    , test "Check add product A+B+C subtotal" <|
        assertEqual (data.productA.price + data.productB.price + data.productC.price) (subtotal .price cart3)
    , test "Check qty 1" <|
        assertEqual 1 (qty cart1)
    , test "Check qty 2" <|
        assertEqual 2 (qty cart2)
    , test "Check qty 3" <|
        assertEqual 3 (qty cart3)
    ]


testInc : List Test
testInc =
  let
    data = setUp
    cart1 = add data.productA data.cart 12345
    cart2a = inc data.productA cart1
    cart2b = inc data.productB cart1
  in
    [ test "Check subtotal 2a" <|
        assertEqual (data.productA.price * 2) (subtotal .price cart2a)
    , test "Check subtotal 2b" <|
        assertEqual data.productA.price (subtotal .price cart2b)
    , test "Check qty 2" <|
        assertEqual 2 (qty cart2a)
    , test "Check qty 1" <|
        assertEqual 1 (qty cart2b)
    ]


testDec : List Test
testDec =
  let
    data = setUp
    cart1 = dec data.productA (inc data.productA (inc data.productA (add data.productA data.cart 12345.0)))
    cart2 = dec data.productB (inc data.productA (inc data.productA (add data.productA data.cart 12345.0)))
  in
    [ test "Check subtotal A x 2" <| assertEqual (data.productA.price * 2) (subtotal .price cart1)
    , test "Check subtotal A x 3" <| assertEqual (data.productA.price * 3) (subtotal .price cart2)
    , test "Check qty 2" <|
        assertEqual 2 (qty cart1)
    , test "Check qty 3" <|
        assertEqual 3 (qty cart2)
    ]


testRemove : List Test
testRemove =
  let
    data = setUp
    cart1 = remove data.productA (inc data.productA (inc data.productA (add data.productA data.cart 12345.0)))
    cart2 = remove data.productB (inc data.productA (inc data.productA (add data.productA data.cart 12345.0)))
  in
    [ test "Check subtotal 0" <| assertEqual 0 (subtotal .price cart1)
    , test "Check qty 0" <| assertEqual 0 (qty cart1)
    , test "Check subtotal A x 3" <| assertEqual (data.productA.price * 3) (subtotal .price cart2)
    , test "Check qty 0" <|
        assertEqual 0 (qty cart1)
    , test "Check qty 3" <|
        assertEqual 3 (qty cart2)
    ]


tests : Test
tests =
    suite "A Cart Test Suite" (concat
      [ testCartItem
      , testAdd
      , testInc
      , testDec
      , testRemove
      ])


main =
    runSuiteHtml tests
