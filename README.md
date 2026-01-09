<p align="center">
  <img src="https://github.com/openpeeps/voodoo/blob/main/.github/voodoo.png" width="170px" height="160px"><br>
  Working with Nim's macros is just Voodoo! A collection of utilities
  to build awesome tools!
</p>

<p align="center">
  <code>nimble install voodoo</code>
</p>


## About
Voodoo is a Nim package that provides a collection of tools and utilities
to build awesome packages and applications using Nim's powerful macro system.


<p align="center">
  <a href="https://github.com/">API reference</a><br>
  <img src="https://github.com/openpeeps/voodoo/workflows/test/badge.svg" alt="Github Actions">  <img src="https://github.com/openpeeps/voodoo/workflows/docs/badge.svg" alt="Github Actions">
</p>

## 😍 Key Features
- Generate fast `getters`/`setters` from object fields
- Make `extensible` enums/objects
- JSON parser with serialization/deserialization support
- JSONL (line-delimited JSON) support
- Agnostic proramming language framework

## Examples
Here are some examples of what you can do with Voodoo.

#### Getters
Generate fast getters from object fields without explicitly writing them. Currently, in Nim you cannot read private fields from other modules. Voodoo's `getters` pragma makes it easy to generate public getters for private fields.

Excluding specfic fields is also supported.

```nim
import pkg/voodoo

type
  Price* {.getters.} = object
    net, gross: string

  Product* {.getters: [id].} = object # exclude one or more fields
    id: string
    title, short_description: string
    price: Price

expandGetters() # is required to expand generated procs.
```

`expandGetters` will generate the following getters:
```nim
proc getNet*(price: Price): string =
  ## Get `net` from `Price`
  result = price.net

proc getGross*(price: Price): string =
  ## Get `gross` from `Price`
  result = price.gross

proc getTitle*(product: Product): string =
  ## Get `title` from `Product`
  result = product.title

proc getShortDescription*(product: Product): string =
  ## Get `short_description` from `Product`
  result = product.short_description

proc getPrices*(product: Product): Price =
  ## Get `price` from `Product`
  result = product.price
```

#### Setters
todo

#### Extensibles
It's easy to make extensible **enums**/**objects**. This is super useful when building frameworks or libraries where users may want to extend your types.

Also, `extensible` pragma works with both public or private definitions

```nim
import voodoo/extensible
type
  Cardinal* {.extensible} = enum
    north, west
```

Done! Now `Cardinal` is an extensible enum. Any other modules/packages importing it can easily add fields to this enum. **Yep, that's voodoo!**

```nim
import voodoo/extensible

# `extendEnum` macro is used to add new fields to an extensible enum.
# it is required to do this before importing the extensible enum's module.
extendEnum Cardinal:
  south
  east

# the extensible enum's module is imported after we setup the extensions.
import ./cardinalModule

assert compiles(Cardinal.north)
assert compiles(Cardinal.south)
assert compiles(Cardinal.east)
```

### Voodoo JSON Parser
Voodoo includes a powerful JSON parser that can serialize and deserialize Nim objects to and from JSON format. It supports various options for customization, such as line-delimited output and skipping default values.

### Voodoo JSON features
- Serialize/Deserialize Nim objects to/from JSON
- JSONL - Line-delimited JSON output
- Pretty printing and Minification
- Date/Time formatting options
- Depth/Size limits for parsing large JSON objects
- Ignore unknown fields during deserialization
- Handle duplicate keys in JSON objects
- Custom Field Mapping
- Scientific notation for numbers (e.g., `-123.456e+2`)

### Voodoo JSON example


Here is an example of how to serialize and deserialize Nim objects using Voodoo's JSON parser:

```nim
import voodoo/parsers/voojson

type
  Address* = object
    city: string
    country: string
    street: string
    zipCode: string

  Person* = object
    name: string
    age: int
    addresses: seq[Address]

# serialize a Nim object to JSON
let p1 = Person(
  name: "John Doe",
  age: 30,
  addresses: @[
    Address(street: "123 Main St"),
    Address(city: "Springfield", country: "USA", zipCode: "12345")
  ])

let str = x.toJson(opts = JsonOptions(
  pretty: true,
  skipFields: @["age"]
))

# deserialize JSON to a Nim object
var p2 = fromJson(str, Person)
assert p1 == p2
```


#### Date/Time formatting
Voodoo's JSON parser supports customizable date/time formatting options. You can specify the desired format for date/time fields during serialization and deserialization.

#### Depth/Size limits
To prevent potential denial-of-service attacks from maliciously crafted JSON inputs, Voodoo's JSON parser includes options to set depth and size limits.


#### Custom Field Mapping
You can define custom mappings for field names during serialization and deserialization. This is useful when the JSON field names differ from the Nim object field names.


### ❤ Contributions & Support
- 🐛 Found a bug? [Create a new Issue](https://github.com/openpeeps/voodoo/issues)
- 👋 Wanna help? [Fork it!](https://github.com/openpeeps/voodoo/fork)
- 😎 [Get €20 in cloud credits from Hetzner](https://hetzner.cloud/?ref=Hm0mYGM9NxZ4)
- 🥰 [Donate via PayPal address](https://www.paypal.com/donate/?hosted_button_id=RJK3ZTDWPL55C)

### 🎩 License
MIT license. [Made by Humans from OpenPeeps](https://github.com/openpeeps).<br>
Copyright &copy; OpenPeeps & Contributors &mdash; All rights reserved.
