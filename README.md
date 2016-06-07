# elm-cart
Elm lang shopping Cart implementation. Provide a way to add/remove product from cart, increment and decrement quantity of product in a cart. Also calculate total quantity and cart subtotal.

```elm
  import Cart

  type alias Product = { id : Int, price : Float, title : String }
  apple = Product 1 10.0 "Apple"
  add apple cart ==
    [{ product = { id = 1, price = 10.0, title = "Apple" }, qty = 1}]
  add apple (add apple cart) ==
    [{ product = { id = 1, price = 10.0, title = "Apple" }, qty = 2}]

```


