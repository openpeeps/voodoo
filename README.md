<p align="center">
  <img src="https://github.com/openpeeps/voodoo/blob/main/.github/voodoo.png" width="160px" height="150px"><br>
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
- [x] Generate fast getters/setters

## Examples
```nim
import pkg/voodoo

type
  Price* {.getters.} = object
    net, gross: string

  Product* {.getters.} = object
    title, short_description: string
    prices: Price
```

### ❤ Contributions & Support
- 🐛 Found a bug? [Create a new Issue](https://github.com/openpeeps/voodoo/issues)
- 👋 Wanna help? [Fork it!](https://github.com/openpeeps/voodoo/fork)
- 😎 [Get €20 in cloud credits from Hetzner](https://hetzner.cloud/?ref=Hm0mYGM9NxZ4)
- 🥰 [Donate via PayPal address](https://www.paypal.com/donate/?hosted_button_id=RJK3ZTDWPL55C)

### 🎩 License
MIT license. [Made by Humans from OpenPeeps](https://github.com/openpeeps).<br>
Copyright &copy; 2024 OpenPeeps & Contributors &mdash; All rights reserved.
