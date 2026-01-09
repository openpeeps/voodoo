import unittest
import ../src/voodoo/parsers/voojson

test "dump array/seq":
  var arr = [1, 2, 3, 4, 5]
  let jsonStr = voojson.toJson(arr)
  check jsonStr == "[1,2,3,4,5]"

  var arr2 = @["apple", "banana", "cherry"]
  let jsonStr2 = voojson.toJson(arr2)
  check jsonStr2 == """["apple","banana","cherry"]"""

test "dump object":
  type
    Person = object
      name: string
      age: int

  var p = Person(name: "Alice", age: 30)
  let jsonStr = voojson.toJson(p)
  check jsonStr == """{name:"Alice",age:30}"""