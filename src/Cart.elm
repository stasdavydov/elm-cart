module Cart exposing
  ( Item, Cart
  , qty, cart
  , add, inc, dec, remove
  , itemSubtotal, subtotal
  )

{-| A tiny library for shopping carts. It supports any Product types to add into a cart.
The ```price``` function is required for cart and item subtotal calculation.

# Types
@docs Item, Cart

# Cart operations
@docs cart, add, inc, dec, remove, subtotal, qty

# Cart Item operations
@docs itemSubtotal

-}


import List exposing (isEmpty, map, foldl, filter, append, sum)
import Time exposing (Time)
import Date exposing (Date)


{-| Item record is a counter of added products into the cart.

    type alias Product = { id : Int, price : Float }
    item = { product = { id = 1, price = 10.0 }, qty = 1 }
-}
type alias Item a =
  { product : a
  , qty : Int
  , date_added : Date
  }


{-| Cart is a list of Items.
-}
type alias Cart a =
  List (Item a)


{-| Return an empty cart.

    cart == []
-}
cart : List (Item a)
cart =
  []


{-| Get a quantity of all products in the cart.

    type alias Product = { id : Int, price : Float }

    qty (cart add <| Product 1 10.0 ) == 1
-}
qty : Cart a -> Int
qty cart =
  foldl (+) 0 (map .qty cart)


{-| Get the item subtotal with the given price function required for getting price of the product.

    type alias Product = { id : Int, price : Float }

    itemSubtotal .price (item <| Product 1 10.0) == 10.0
-}
itemSubtotal : (a -> Float) -> Item a -> Float
itemSubtotal price_f item =
  price_f item.product * toFloat item.qty


{-| Add the product to the cart. Time of adding is specified as a parameter.

    type alias Product = { id : Int, price : Float }

    add (Product 1 10.0) cart 12345 ==
      [{ product = { id = 1, price = 10.0 }, qty = 1, date_added = 12345 }]
-}
add : a -> Cart a -> Time -> Cart a
add product cart time =
    if (isEmpty cart) || isEmpty (filter (\i -> i.product == product) cart)
      then append cart [Item product 1 (Date.fromTime time) ]
      else inc product cart


{-| Increase quantity of the product in the cart.

    type alias Product = { id : Int, price : Float }
    p = Product 1 10.0

    c = add p cart 12345
    c == [{ product = { id = 1, price = 10.0 }, qty = 1, date_added = 12345 }]

    inc p c == [{ product = { id = 1, price = 10.0 }, qty = 2, date_added = 12345 }]
-}
inc : a -> Cart a -> Cart a
inc product cart =
  let
    updateQty i =
      if i.product == product then { i | qty = i.qty + 1 } else i
  in
    map updateQty cart


{-| Decrease quantity of the product in the cart.

    type alias Product = { id : Int, price : Float }
    p = Product 1 10.0
    c = inc p (add p cart 12345)

    c == [{ product = { id = 1, price = 10.0 }, qty = 2, date_added = 12345 }]

    dec p c == [{ product = { id = 1, price = 10.0 }, qty = 1 }]
    dec p (dec p c) == []
-}
dec : a -> Cart a -> Cart a
dec product cart =
  let
    isLastItem i =
      if i.qty == 1 && i.product == product then False else True

    decItem i =
      if i.product == product then { i | qty = i.qty - 1 } else i

  in
    map decItem <| filter isLastItem cart


{-| Remove the product from the cart.

    type alias Product = { id : Int, price : Float }
    p = Product 1 10.0
    c = add p cart 12345
    c == [{ product = { id = 1, price = 10.0 }, qty = 1, date_added = 12345 }]
    remove p c == []
-}
remove : a -> Cart a -> Cart a
remove product cart =
  filter (\i -> i.product /= product) cart


{-| Get subtotal of the cart with the given price function for getting price of stored products in the cart.

    type alias Product = { id : Int, price : Float }
    p = Product 1 10.0
    subtotal .price (inc p (add p cart 12345)) == 20.0

-}
subtotal : (a -> Float) -> Cart a -> Float
subtotal price_f cart =
  sum <| map (itemSubtotal price_f) cart
