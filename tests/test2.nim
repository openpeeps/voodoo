import unittest
import voodoo/extensibles

extendEnum Cardinal:
  south
  east

extendEnum Cardinal:
  unknown

import ./extensibleModule

assert compiles Cardinal.north
assert compiles Cardinal.west
assert compiles Cardinal.south
assert compiles Cardinal.east
assert compiles Cardinal.unknown