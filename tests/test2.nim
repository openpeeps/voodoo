import unittest
import voodoo/extensibles

extendEnum Cardinal:
  south
  east

import ./extensibleModule

assert compiles Cardinal.south
assert compiles Cardinal.east