import unittest
import voodoo

type
  Price {.getters.} = object
    net, gross: string

  Product {.getters: [id].} = object
    id: string
    title, short_description: string
    prices: seq[Price]

expandGetters()

test "expand getters":
  # Price getters
  check compiles getNet Price()
  check compiles getGross Price()

  # Product getters
  check not compiles getId(Product()) # excluded
  check compiles getTitle Product()
  check compiles getShortDescription Product()
  check compiles getPrices Product()