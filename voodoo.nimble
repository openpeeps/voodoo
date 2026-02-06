# Package

version       = "0.1.0"
author        = "George Lemon"
description   = "Working with Nim's macros is just Voodoo"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 2.0.0"
requires "libffi"


task oop, "test oop":
  exec "nim c --out:./bin/oop src/voodoo/oop.nim"