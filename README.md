<p align="center">
  <img src="https://github.com/openpeeps/voodoo/blob/main/.github/voodoo.png" width="170px" height="160px"><br>
  Working with Nim's macros 👑 is just Voodoo
</p>

<p align="center">
  <code>nimble install voodoo</code>
</p>

<p align="center">
  <a href="https://github.com/">API reference</a><br>
  <img src="https://github.com/openpeeps/voodoo/workflows/test/badge.svg" alt="Github Actions">  <img src="https://github.com/openpeeps/voodoo/workflows/docs/badge.svg" alt="Github Actions">
</p>

## 😍 Key Features
- [x] Compile-time utility tool
- [x] Helps reduce code size & repetitive tasks
- [x] Generate fast `getters`/`setters`
- [x] Callee Introspection

## Examples

#### Getters
Generate fast getters from object fields. Excluding fields is also possible.
```nim
import pkg/voodoo

type
  Price* {.getters.} = object
    net, gross: string

  Product* {.getters: [id].} = object # exclude one or more fields
    id: string
    title, short_description: string
    prices: Price

expandGetters() # is required to expand generated procs.
```

Output:
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

proc getPrices*(product: Product): seq[Price] =
  ## Get `prices` from `Product`
  result = product.prices
```

#### Setters
todo

#### Callee 
todo

### ❤ Contributions & Support
- 🐛 Found a bug? [Create a new Issue](https://github.com/openpeeps/voodoo/issues)
- 👋 Wanna help? [Fork it!](https://github.com/openpeeps/voodoo/fork)
- 😎 [Get €20 in cloud credits from Hetzner](https://hetzner.cloud/?ref=Hm0mYGM9NxZ4)
- 🥰 [Donate via PayPal address](https://www.paypal.com/donate/?hosted_button_id=RJK3ZTDWPL55C)

### 🎩 License
MIT license. [Made by Humans from OpenPeeps](https://github.com/openpeeps).<br>
Copyright &copy; 2024 OpenPeeps & Contributors &mdash; All rights reserved.
