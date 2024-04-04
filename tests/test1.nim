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

# Price getters
assert compiles getNet Price()
assert compiles getGross Price()

# Product getters
assert not compiles getId(Product()) # excluded
assert compiles getTitle Product()
assert compiles getShortDescription Product()
assert compiles getPrices Product()