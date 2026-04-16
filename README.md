<p align="center">
  <img src="https://github.com/openpeeps/voodoo/blob/main/.github/voodoo.png" width="170px" height="160px"><br>
  Working with Nim's macros is just Voodoo!<br>
  A collection of utilities to build awesome tools!
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

### ❤ Contributions & Support
- 🐛 Found a bug? [Create a new Issue](https://github.com/openpeeps/voodoo/issues)
- 👋 Wanna help? [Fork it!](https://github.com/openpeeps/voodoo/fork)
- 😎 [Get €20 in cloud credits from Hetzner](https://hetzner.cloud/?ref=Hm0mYGM9NxZ4)

### 🎩 License
MIT license. [Made by Humans from OpenPeeps](https://github.com/openpeeps).<br>
Copyright &copy; OpenPeeps & Contributors &mdash; All rights reserved.
