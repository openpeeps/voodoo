import unittest, json
import ../src/voodoo/parsers/voojson

test "parse jsonl":

  let objects = """
{"name": "Gilbert", "session": "2013", "score": 24, "completed": true}
{"name": "Alexa", "session": "2013", "score": 29, "completed": true}
{"name": "May", "session": "2012B", "score": 14, "completed": false}
{"name": "Deloise", "session": "2012A", "score": 19, "completed": true} 
  """
  let data = fromJsonL(objects)
  check data.kind == JArray
  check data.len == 4
  for item in data:
    assert item.kind == JObject
    check item["name"].kind == JString
    check item["session"].kind == JString
    check item["score"].kind == JInt
    check item["completed"].kind == JBool

  let arrayData = """
["Name", "Session", "Score", "Completed"]
["Gilbert", "2013", 24, true]
["Alexa", "2013", 29, true]
["May", "2012B", 14, false]
["Deloise", "2012A", 19, true] 
  """
  let arrData = fromJsonL(arrayData)
  check arrData.kind == JArray
  check arrData.len == 5
  for item in arrData:
    assert item.kind == JArray
    check item.len == 4

  check arrData[0][3].kind == JString
  check arrData[4][3].kind == JBool

  let nestedData = """
{"name": "Gilbert", "wins": [["straight", "7♣"], ["one pair", "10♥"]]}
{"name": "Alexa", "wins": [["two pair", "4♠"], ["two pair", "9♠"]]}
{"name": "May", "wins": []}
{"name": "Deloise", "wins": [["three of a kind", "5♣"]]}
  """
  let nested = fromJsonL(nestedData)
  check nested.kind == JArray
  check nested.len == 4
  for player in nested:
    check player.kind == JObject
    check player["name"].kind == JString
    check player["wins"].kind == JArray
    for win in player["wins"]:
      check win.kind == JArray
      check win.len == 2
      check win[0].kind == JString
      check win[1].kind == JString