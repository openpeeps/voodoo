# Working with Nim's macros is just Voodoo
#
# (c) 2025 George Lemon | MIT License
#          Made by Humans from OpenPeeps
#          https://github.com/openpeeps/voodoo

## This module implements a JSON parser and serializer for Nim language.
## 
## It can convert Nim objects, tables and arrays to JSON strings and vice versa.
## It also provides compile-time options for customizing the serialization process.

import std/[macros, macrocache, json, strutils, options]

type
  Integers* = int | int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64
  
  JsonOptions* = ref object
    ## Options for JSON serialization
    skipFields*: seq[string]
      ## Fields to skip during serialization
    skipNulls: bool = false
      ## Whether to skip fields that are null during serialization
    lineDelimited: bool = false
      ## Whether to output JSON in a line-delimited format
      ## This is useful for large datasets where each JSON object is on a new line
  
  VoodooParsingError* = object of CatchableError

template skippable*() {.pragma.}

# Forward declarations
proc objectToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode
proc arrayToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode
proc nimToJson*(val: string): string
proc nimToJson*(val: Integers): string
proc nimToJson*(val: float32|float64): string
proc nimToJson*(val: bool): string
proc nimToJson*(val: ref object): string
proc nimToJson*(val: object): string
      
macro toJson*(v: typed, opts: static JsonOptions = nil): untyped =
  ## Converts a Nim object to its JSON representation.
  # retrieve the implementation of typed `v`
  var valImpl = v.getType()
  var objName = v.getTypeInst()
  case valImpl.kind:
  of nnkObjectTy:
    return objectToJson(v, valImpl, opts)
  of nnkBracketExpr:
    let ty = v.getTypeImpl()
    if ty.kind == nnkRefTy:
      return objectToJson(v, valImpl[1].getType(), opts)
    # sequences or arrays
    return arrayToJson(v, valImpl, opts)
  else: discard

proc nimToJson*[T](arr: seq[T]): string = 
  ## Converts a sequence of items to a JSON array string.
  result = "["
  for i, item in arr:
    if i > 0: result.add(",")
    result.add(item.nimToJson())
  result.add("]")

proc nimToJson*(val: string): string = 
  ## Converts a string to JSON
  result = "\"" & val & "\""

proc nimToJson*(val: Integers): string = 
  ## Converts int to JSON
  result = $val

proc nimToJson*(val: float32|float64): string = 
  ## Converts float to JSON
  result = $val

proc nimToJson*(val: bool): string = 
  ## Converts a bool to JSON
  result = $val

proc nimToJson*(val: ref object): string = 
  ## Converts a ref object to JSON
  if val == nil: return "null"
  toJson(val)

proc nimToJson*(val: object): string = 
  ## Converts a ref object to JSON
  toJson(val)

proc objectToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode =
  let strObjFields = newStmtList()
  var i = 0
  let res = genSym(nskVar, "res")
  var len = valImpl[2].len
  for field in valImpl[2]:
    case field.kind
    of nnkSym:
      let fieldName: string = field.repr
      if fieldName in opts.skipFields:
        # skip any field that is mentioned
        # in the skipFields option
        inc i
        continue
      let fieldTree: NimNode = field.getType()
      var strKeyVal: NimNode =
        nnkInfix.newTree(
          ident"&",
          newLit("\"" & fieldName & "\":"),
          newCall(ident"nimToJson", newDotExpr(v, ident(fieldName)))
        )
      if i != 0 and i < len:
        strObjFields.add(newCall(ident"add", res, newLit(",")))
      strObjFields.add(newCall(ident"add", res, strKeyVal))
      inc i
    of nnkRecCase:
      discard # handle object variants
    else: discard
  var objectSerialization = genSym(nskLabel, "objectSerialization")
  result = newStmtList()
  result.add quote do:
    block `objectSerialization`:
      var `res` = "{"
      `strObjFields`
      `res`.add("}")
      `res`

proc arrayToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode =
  ## Converts a Nim array or sequence to its JSON representation.
  var strArrayItems = newStmtList()
  var res = genSym(nskVar, "voodooArray")
  var blockLabel = genSym(nskLabel, "arraySerialization")
  result = newStmtList()
  result.add quote do:
    block `blockLabel`:
      var `res` = "["
      var i = 0
      for item in `v`:
        if i != 0:
          `res`.add(',')
        `res`.add(nimToJson(item))
        inc i
      `res`.add("]")
      `res`

#
# JSON Parser
#
type
  TokenKind = enum
    tkEof,
    tkLBrace = "{", tkRBrace = "}", tkLBracket = "[", tkRBracket = "]",
    tkComma = ",", tkColon = ":",
    tkString, tkNumber, tkTrue, tkFalse, tkNull = "null"

  Lexer = object
    input: string
    pos: int
    line, col: int
    current: char

  Token = object
    kind: TokenKind
    value: Option[string]
    line, col: int

  LexerState = object
    pos: int
    line: int
    col: int
    current: char
    
  Parser = object
    lexer: Lexer
    currentToken: Token

proc newLexer(input: string): Lexer =
  result = Lexer(input: input, pos: 0, line: 1, col: 1)
  if input.len > 0:
    result.current = input[0]

proc advance(l: var Lexer) =
  if l.pos < l.input.len - 1:
    inc l.pos
    l.current = l.input[l.pos]
    inc l.col
  else:
    l.current = '\0'

proc peekChar(l: Lexer): char =
  if l.pos + 1 < l.input.len:
    result = l.input[l.pos + 1]
  else:
    result = '\0'

proc skipWhitespace(l: var Lexer) =
  while true:
    case l.current
    of {' ', '\t', '\n', '\r'}:
      if l.current == '\n':
        inc l.line
        l.col = 0
      advance(l)
    else: break

proc readString(l: var Lexer): string =
  result = ""
  while l.current != '"':
    result.add(l.current)
    advance(l)
  advance(l) # skip the closing quote

proc readNumber(l: var Lexer): string =
  result = ""
  if l.current == '-':
    result.add('-')
    advance(l)
  while l.current in {'0'..'9'}:
    result.add(l.current)
    advance(l)
  if l.current == '.':
    result.add('.')
    advance(l)
    while l.current in {'0'..'9'}:
      result.add(l.current)
      advance(l)
  if l.current in {'e', 'E'}:
    result.add(l.current)
    advance(l)
    if l.current in {'+', '-'}:
      result.add(l.current)
      advance(l)
    while l.current in {'0'..'9'}:
      result.add(l.current)
      advance(l)

const
  invalidToken = "Invalid token `$1`"
  errorEndOfFile = "Unexpected EOF while parsing `$1`"
  unexpectedToken = "Unexpected token `$1`"
  unexpectedChar = "Unexpected character `$1`"

proc error(l: var Lexer, msg: string) =
  # Raise a lexer error
  raise newException(VoodooParsingError, ("Error ($1:$2) " % [$l.line, $l.col]) & msg)

proc error(p: var Parser, msg: string) =
  # Raise a parsing error with the current lexer position
  raise newException(VoodooParsingError, ("Error ($1:$2) " % [$p.lexer.line, $p.lexer.col]) & msg)

proc nextToken(l: var Lexer): Token =
  # Get the next token from the lexer
  skipWhitespace(l)
  result = Token(line: l.line, col: l.col)
  case l.current
  of '\0':
    result.kind = tkEof
  of '{':
    result.kind = tkLBrace
    advance(l)
  of '}':
    result.kind = tkRBrace
    advance(l)
  of '[':
    result.kind = tkLBracket
    advance(l)
  of ']':
    result.kind = tkRBracket
    advance(l)
  of ',':
    result.kind = tkComma
    advance(l)
  of ':':
    result.kind = tkColon
    advance(l)
  of '"':
    advance(l)  # skip the opening quote
    result.kind = tkString
    result.value = some(readString(l))
  of '0'..'9', '-':
    result.kind = tkNumber
    result.value = some(readNumber(l))
  of 't':
    if l.input[l.pos..l.pos + 3] == "true":
      result.kind = tkTrue
      l.pos += 4
      l.col += 4
      l.current = l.input[l.pos]
    else:
      l.error(invalidToken % $l.current)
  of 'f':
    if l.input[l.pos..l.pos + 4] == "false":
      result.kind = tkFalse
      l.pos += 5
      l.col += 5
      l.current = l.input[l.pos]
    else:
      l.error(invalidToken % $l.current)
  of 'n':
    if l.input[l.pos..l.pos + 3] == "null":
      result.kind = tkNull
      l.pos += 3
      l.col += 3
      l.current = l.input[l.pos]
    else: 
      l.error(unexpectedToken % [l.input[l.pos..l.pos + 3]])
  else:
    l.error(unexpectedChar % [$l.current])

#
# Forward decl for JSON parsing
#
proc parseObject(parser: var Parser, obj: var JsonNode)
proc parseArray(parser: var Parser, arr: var JsonNode)

proc parseObject(parser: var Parser, obj: var JsonNode) =
  # Parse a JSON object
  while true:
    let token = parser.lexer.nextToken()
    case token.kind
    of tkEOF:
      raise newException(ValueError, "EOF reached while parsing object")
    of tkRBrace:
      break # end of object
    of tkString:
      let key = token.value.get()
      let colonToken = parser.lexer.nextToken()
      if colonToken.kind != tkColon:
        raise newException(ValueError, "Expected ':' after key '" & key & "' at line " & $token.line & ", column " & $token.col)
      let valToken = parser.lexer.nextToken()
      case valToken.kind
        of tkString:
          obj[key] = newJString(valToken.value.get())
        of tkNumber:
          obj[key] = %*(valToken.value.get())
        of tkTrue, tkFalse:
          obj[key] = newJBool(valToken.kind == tkTrue)
        of tkNull:
          obj[key] = newJNull()
        of tkLBrace:
          var nestedObj = newJObject()
          parser.parseObject(nestedObj)
          obj[key] = nestedObj
        of tkLBracket:
          var nestArr = newJArray()
          parser.parseArray(nestArr)
          obj[key] = nestArr
        else:
          parser.error(unexpectedToken % [$valToken.kind])
    of tkComma:
      continue
    else:
      parser.error(unexpectedToken % [$token.kind])

proc parseArray(parser: var Parser, arr: var JsonNode) =
  # Parse a JSON array
  while true:
    let token = parser.lexer.nextToken()
    case token.kind
    of tkEOF:
      parser.error(errorEndOfFile % "array")
    of tkRBracket:
      break # end of array
    of tkString:
      arr.add(newJString(token.value.get()))
    of tkNumber:
      arr.add(%*(token.value.get()))
    of tkTrue, tkFalse:
      arr.add(newJBool(token.kind == tkTrue))
    of tkNull:
      arr.add(newJNull())
    of tkLBrace:
      var nestedObj = newJObject()
      parser.parseObject(nestedObj)
      arr.add(nestedObj)
    of tkComma:
      continue
    else:
      parser.error(unexpectedToken % [$token.kind])

proc parseJson(parser: var Parser, v: var JsonNode) =
  # Parse a JSON string and fill the `JsonNode` structure
  while true:
    let token = parser.lexer.nextToken()
    case token.kind
    of tkEOF: break # end of file
    of tkLBrace:
      var obj = newJObject()
      parser.parseObject(obj)
      if v != nil:
        v.add(obj)
      else:
        v = obj
    of tkLBracket:
      var arr = newJArray()
      parser.parseArray(arr)
      if v != nil:
        v.add(arr)
      else:
        v = arr
    else: discard

proc fromJson*(str: string): JsonNode =
  ## Parse a JSON from `str` and returns the standard `JsonNode`
  var parser = Parser(lexer: newLexer(str))
  parser.parseJson(result)

proc fromJson*[T](str: string, obj: typedesc[T]): T =
  ## Provide a direct to object conversion from JSON string to Nim objects
  discard # todo
