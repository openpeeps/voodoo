import unittest
import voodoo/extensibles

extendEnum Cardinal:
  south
  east

extendEnum Cardinal:
  unknown

import ./extensibleModule

test "extensible enums":
  check compiles Cardinal.north
  check compiles Cardinal.west
  check compiles Cardinal.south
  check compiles Cardinal.east
  check compiles Cardinal.unknown