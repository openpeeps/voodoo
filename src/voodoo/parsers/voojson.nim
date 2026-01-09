# Working with Nim's macros is just Voodoo
#
# (c) 2026 George Lemon | MIT License
#          Made by Humans from OpenPeeps
#          https://github.com/openpeeps/voodoo

## This module implements a JSON parser and serializer for Nim language.
## 
## It can convert Nim objects, tables and arrays to JSON strings and vice versa.
## It also provides compile-time options for customizing the serialization process.

import std/[macros, macrocache, json, sequtils,
        strutils, options, tables, enumutils]

type
  Integers* = int | int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64
  
  JsonOptions* = ref object
    ## Options for JSON serialization
    pretty: bool
      ## Whether to pretty-print the JSON output
    skipFields*: seq[string]
      ## Fields to skip during serialization
    skipNulls: bool
      ## Whether to skip fields that are null
      ## during serialization
    lineDelimited: bool
      ## Whether to output JSON in a line-delimited
      ## format. This is useful for large datasets
      ## where each JSON object is on a new line
    skipDefaults: bool
      ## Whether to skip fields that have default
      ## values during serialization
    skipUnknownFields: bool = true
      ## Whether to ignore unknown fields during
      ## deserialization

  #
  # JSON Parser
  #
  TokenKind = enum
    tkEof,
    tkLBrace = "{"
    tkRBrace = "}"
    tkLBracket = "["
    tkRBracket = "]"
    tkComma = ","
    tkColon = ":"
    tkString, tkNumber, tkTrue, tkFalse, tkNull = "null"

  Lexer = ref object
    input: string
    pos: int
    line, col: int
    current: char

  Token = ref object
    kind: TokenKind
    value: Option[string]
    line, col: int
    
  Parser = object
    lexer: Lexer
    prev, curr, next: Token
    options: JsonOptions

  VoodooParsingError* = object of CatchableError

template skippable*() {.pragma.}

# Forward declarations
proc objectToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode
proc arrayToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode

proc dumpHook*(s: var string, val: string)
proc dumpHook*(s: var string, val: Integers)
proc dumpHook*(s: var string, val: float32|float64)
proc dumpHook*(s: var string, val: bool)
proc dumpHook*(s: var string, val: ref object)
proc dumpHook*(s: var string, val: object)

proc parseJson(parser: var Parser, v: var object)

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

proc dumpHook*[T](s: var string, arr: seq[T]) = 
  ## Converts a sequence of items to a JSON array string.
  s.add("[")
  for i, item in arr:
    if i > 0: s.add(",") # add comma between items
    s.dumpHook(item)
  s.add("]")

proc dumpHook*(s: var string, val: string) = 
  ## Converts a string to JSON
  s.add("\"" & val & "\"")

proc dumpHook*(s: var string, val: Integers) = 
  ## Converts int to JSON
  s.add($val)

proc dumpHook*(s: var string, val: float32|float64) = 
  ## Converts float to JSON
  s.add($val)

proc dumpHook*(s: var string, val: bool) = 
  ## Converts a bool to JSON
  s.add($val)

proc dumpHook*(s: var string, val: ref object) = 
  ## Converts a ref object to JSON
  if val == nil: s.add("null")
  dumpHook(s, val)

proc dumpHook*(s: var string, val: object) = 
  ## Converts a ref object to JSON
  dumpHook(s, val)

proc objectToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode =
  let strObj = newStmtList()
  var i = 0
  let res = genSym(nskVar, "res")
  var len = valImpl[2].len
  for field in valImpl[2]:
    case field.kind
    of nnkSym:
      let fieldName = field.strVal
      if opts != nil:
        if opts.skipFields.len > 0 and opts.skipFields.contains(fieldName):
          # skip any field that is mentioned
          # in the skipFields option
          inc i
          continue
      # var strKeyVal: NimNode =
      #   nnkInfix.newTree(
      #     ident"&",
      #     newLit("\"" & fieldName & "\":"),
      #     newCall(ident"dumpHook",  newDotExpr(v, ident(fieldName)))
      #   )
      if i != 0 and i < len:
        strObj.add(newCall(ident"add", res, newLit(",")))
      strObj.add(newCall(ident"add", res, newLit(fieldName & ":")))
      strObj.add(
        newCall(ident("dumpHook"), res, newDotExpr(v, ident(fieldName)))
      )
      inc i
    of nnkRecCase:
      discard # handle object variants
    else: discard
  var objectSerialization = genSym(nskLabel, "objectSerialization")
  result = newStmtList()
  result.add quote do:
    block `objectSerialization`:
      var `res` = "{"
      `strObj`
      `res`.add("}")
      `res`

proc arrayToJson*(v, valImpl: NimNode, opts: JsonOptions = nil): NimNode =
  ## Converts a Nim array or sequence to its JSON representation.
  var strArrayItems = newStmtList()
  var blockLabel = genSym(nskLabel, "VoodooArraySerialization")
  result = newStmtList()
  # echo v.getImpl().treerepr
  result.add quote do:
    block `blockLabel`:
      var str: string
      str.add("[")
      if `v`.len > 0:
        dumpHook(str, `v`[0]) # first item without comma
        for i, item in `v`[1..^1]:
          str.add(",")
          dumpHook(str, item)
      str.add("]")
      move(str) # return the JSON string
#
# JSON Parser
#
proc `$`*(tk: TokenKind): string =
  ## Convert TokenKind to string
  result = 
    case tk
    of tkLBrace, tkRBrace, tkLBracket,
        tkRBracket, tkComma, tkColon:
          tk.symbolName
    of tkEof: "<EOF>"
    of tkString: "<string>"
    of tkNumber: "<number>"
    of tkTrue, tkFalse: "<boolean>"
    of tkNull: "<null>"

proc `$`*(tk: Token): string =
  ## Convert Token to string
  result = "TOKEN<kind: " & $tk.kind & 
           (if tk.value.isSome: ", value:" & tk.value.get() else: "") & 
           ", line:" & $tk.line & ", col:" & $tk.col & ">"

proc nextToken(parser: var Parser): Token {.discardable.}

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

proc peekChar(l: var Lexer): char =
  # Peek the next character without advancing the lexer
  if l.pos + 1 < l.input.len:
    result = l.input[l.pos + 1]
  else:
    result = '\0'

proc peekUntil(parser: var Parser, stopChar: char): string =
  # Peek characters until stopChar without advancing the lexer
  var tempPos = parser.lexer.pos
  result = ""
  while tempPos < parser.lexer.input.len and parser.lexer.input[tempPos] != stopChar:
    result.add(parser.lexer.input[tempPos])
    inc tempPos

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
    # scientific notation
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
  unexpectedTokenExpected = "Got `$1`, expected $2"
  unexpectedChar = "Unexpected character `$1`"

proc error(l: var Lexer, msg: string) =
  # Raise a lexer error
  raise newException(VoodooParsingError, ("Error ($1:$2) " % [$l.line, $l.col]) & msg)

proc error(p: var Parser, msg: string) =
  # Raise a parsing error with the current lexer position
  raise newException(VoodooParsingError, ("Error ($1:$2) " % [$p.lexer.line, $p.lexer.col]) & msg)

proc nextToken(parser: var Parser): Token =
  # Get the next token from the lexer
  skipWhitespace(parser.lexer)
  result = Token(line: parser.lexer.line, col: parser.lexer.col)
  case parser.lexer.current
  of '\0':
    result.kind = tkEof
  of '{':
    result.kind = tkLBrace
    advance(parser.lexer)
  of '}':
    result.kind = tkRBrace
    advance(parser.lexer)
  of '[':
    result.kind = tkLBracket
    advance(parser.lexer)
  of ']':
    result.kind = tkRBracket
    advance(parser.lexer)
  of ',':
    result.kind = tkComma
    advance(parser.lexer)
  of ':':
    result.kind = tkColon
    advance(parser.lexer)
  of '"':
    advance(parser.lexer)  # skip the opening quote
    result.kind = tkString
    result.value = some(readString(parser.lexer))
  of '0'..'9', '-':
    result.kind = tkNumber
    result.value = some(readNumber(parser.lexer))
  of 't':
    if parser.lexer.input[parser.lexer.pos..parser.lexer.pos + 3] == "true":
      result.kind = tkTrue
      parser.lexer.pos += 4
      parser.lexer.col += 4
      parser.lexer.current = parser.lexer.input[parser.lexer.pos]
    else:
      parser.lexer.error(invalidToken % $parser.lexer.current)
  of 'f':
    if parser.lexer.input[parser.lexer.pos..parser.lexer.pos + 4] == "false":
      result.kind = tkFalse
      parser.lexer.pos += 5
      parser.lexer.col += 5
      parser.lexer.current = parser.lexer.input[parser.lexer.pos]
    else:
      parser.lexer.error(invalidToken % $parser.lexer.current)
  of 'n':
    if parser.lexer.input[parser.lexer.pos..parser.lexer.pos + 3] == "null":
      result.kind = tkNull
      parser.lexer.pos += 3
      parser.lexer.col += 3
      parser.lexer.current = parser.lexer.input[parser.lexer.pos]
    else: 
      parser.lexer.error(unexpectedToken % [parser.lexer.input[parser.lexer.pos..parser.lexer.pos + 3]])
  else:
    parser.lexer.error(unexpectedChar % [$parser.lexer.current])

proc walk(parser: var Parser): Token {.discardable.} =
  # Advance to the next token and return it
  parser.prev = parser.curr
  parser.curr = parser.next
  parser.next = parser.nextToken()
  result = parser.curr


#
# JSON Parsing implementation to Nim objects
#

proc parseHook*(parser: var Parser, field: string, v: var string)
proc parseHook*[T: int|int32|int64](parser: var Parser, field: string, v: var T)
proc parseHook*[T: float|float32|float64](parser: var Parser, field: string, v: var T)
proc parseHook*(parser: var Parser, field: string, v: var bool)
proc parseHook*[T](parser: var Parser, field: string, v: var seq[T])
proc parseHook*(parser: var Parser, field: string, v: var ref object)
# proc parseHook*[T: enum](parser: var Parser, field: string, v: var T)

proc skipValue*(parser: var Parser)

proc expectSkip(parser: var Parser, tkind: TokenKind) =
  if parser.curr.kind != tkind:
    if parser.curr.kind == tkEof:
      parser.error(errorEndOfFile % $parser.curr.kind)
    else:
      parser.error(unexpectedTokenExpected % [$parser.curr.kind, $tkind])
  else:
    parser.walk()

template withKeyValue(body: untyped) {.inject.} =
  let key = parser.curr
  parser.walk()
  parser.expectSkip(tkColon)
  var token = parser.curr
  parser.walk()
  body
  if parser.curr.kind == tkComma:
    parser.walk()

template withKey(body: untyped) {.inject.} =
  let key = parser.walk()
  parser.expectSkip(tkColon)
  body
  if parser.next.kind == tkComma:
    parser.walk()
  
#
# Skip Values
#
proc skipValue*(parser: var Parser) =
  ## Skip the current value in the parser
  case parser.curr.kind
  of tkLBrace:
    # skip object
    while parser.curr.kind != tkRBrace:
      parser.walk()
      skipValue(parser)
      if parser.curr.kind == tkComma:
        parser.walk()
    parser.expectSkip(tkRBrace)
  of tkLBracket:
    # skip array
    while parser.curr.kind != tkRBracket:
      parser.walk()
      skipValue(parser)
      if parser.curr.kind == tkComma:
        parser.walk()
    parser.expectSkip(tkRBracket)
  else:
    while parser.curr.kind notin {tkComma, tkRBrace, tkRBracket, tkEof}:
      parser.walk()

#
# Parse Hooks
#
proc parseHook*(parser: var Parser, field: string, v: var string) =
  ## A hook to parse string fields
  v = parser.curr.value.get()

proc parseHook*[T: int|int32|int64](parser: var Parser, field: string, v: var T) =
  ## A hook to parse integer fields
  v = parser.curr.value.get().parseInt()

proc parseHook*[T: float|float32|float64](parser: var Parser, field: string, v: var T) =
  ## A hook to parse integer fields
  v = parser.curr.value.get().parseFloat()

proc parseHook*(parser: var Parser, field: string, v: var bool) =
  ## A hook to parse boolean fields
  v = parser.curr.kind == tkTrue

macro getObjectFields(obj: typed): untyped =
  let tempFields = obj.getType()[2]
  var fields =
    if tempFields.len > 0 and tempFields[0].kind == nnkRecList:
      tempFields[0]
    else:
      tempFields
  var fieldList = newNimNode(nnkBracket)
  for field in fields:
    case field.kind
    of nnkSym:
      fieldList.add(newLit(field.strVal))
    else: discard
  result = newStmtList().add(nnkPrefix.newTree(ident"@", fieldList))

proc parseHook*(parser: var Parser, field: string, v: var object) =
  ## Custom parsing for Address field
  parser.expectSkip(tkLBrace) # start of object
  const objectFields: seq[string] = getObjectFields(v)
  while parser.curr.kind notin {tkRBrace, tkEof}:
    var key: string
    block all:
      # find the matching field in the object
      if objectFields.len > 0:
        key = parser.curr.value.get()
        for objField, objVal in v.fieldPairs:
          if key == objField:
            parser.walk() # advance to field name
            parser.expectSkip(tkColon)
            # call the appropriate parseHook based on field type
            parser.parseHook(objField, objVal)
            if parser.curr.kind != tkRBrace:
              parser.walk() # advance to next token
            break all
          else:
            if key notin objectFields:
              # TODO skip or error based on options
              parser.skipValue()
      else:
        # the object has no fields, skip parsing
        parser.skipValue()
    if parser.curr.kind == tkComma:
      parser.walk() # skip comma
  # the end of the object
  parser.expectSkip(tkRBrace)

proc parseHook*(parser: var Parser, field: string, v: var ref object) =
  ## A hook to parse ref object fields
  if parser.curr.kind == tkNull:
    parser.walk() # skip null
  else:
    # create a new instance of the ref object
    v = new(type(v))
    parser.parseHook("", v[])

proc parseHook*[T](parser: var Parser, field: string, v: var seq[T]) = 
  ## A hook to parse sequence fields
  parser.expectSkip(tkLBracket) # start of array
  var item: T
  while parser.curr.kind != tkRBracket:
    parser.parseHook("", item)
    v.add(item)
    if parser.curr.kind == tkComma:
      parser.walk()
  parser.expectSkip(tkRBracket) # end of array

#
# JsonNode Objects
#
#
# Forward decl for JSON parsing
#
proc parseObject(parser: var Parser, obj: var JsonNode)
proc parseArray(parser: var Parser, arr: var JsonNode)

#
# JSON Parsing Implementation
#
proc parseObject(parser: var Parser, obj: var JsonNode) =
  # Parse a JSON object
  while parser.curr.kind != tkRBrace:
    let token = parser.walk()
    case token.kind
    of tkEOF:
      raise newException(ValueError, "EOF reached while parsing object")
    of tkString:
      let key = token.value.get()
      let colonToken = parser.walk()
      if colonToken.kind != tkColon:
        raise newException(ValueError,
          "Expected ':' after key '" & key & "' at line " & $token.line & ", column " & $token.col)
      let valToken = parser.walk()
      case valToken.kind
        of tkString:
          obj[key] = newJString(valToken.value.get())
        of tkNumber:
          let num =
            try:
              newJInt(parseInt(valToken.value.get()))
            except ValueError:
              newJFloat(parseFloat(valToken.value.get()))
          obj[key] = num
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
    of tkComma, tkRBrace:
      continue
    else:
      parser.error(unexpectedToken % [$token.kind])
  parser.walk() # consume the closing '}'

proc parseArray(parser: var Parser, arr: var JsonNode) =
  # Parse a JSON array to JsonNode
  while parser.curr.kind != tkRBracket:
    let token = parser.walk()
    case token.kind
    of tkEOF:
      parser.error(errorEndOfFile % "array")
    of tkLBracket:
      # nested array
      var nestedArr = newJArray()
      parser.parseArray(nestedArr)
      arr.add(nestedArr)
    of tkString:
      arr.add(newJString(token.value.get()))
    of tkNumber:
      let num =
        try:
          newJInt(parseInt(token.value.get()))
        except ValueError:
          newJFloat(parseFloat(token.value.get()))
      arr.add(num)
    of tkTrue, tkFalse:
      arr.add(newJBool(token.kind == tkTrue))
    of tkNull:
      arr.add(newJNull())
    of tkLBrace:
      var nestedObj = newJObject()
      parser.parseObject(nestedObj)
      arr.add(nestedObj)
    of tkComma, tkRBracket:
      continue
    else:
      parser.error(unexpectedToken % [$token.kind])
  parser.walk() # consume the closing ']'

proc fromJson*(str: string): JsonNode =
  ## Parse a JSON from `str` and returns the standard `JsonNode`
  var parser = Parser(lexer: newLexer(str))
  parser.curr = parser.nextToken()
  parser.next = parser.nextToken()
  case parser.curr.kind
  of tkLBrace:
    result = newJObject()
    parser.parseObject(result)
  else:
    parser.error(unexpectedToken % [$parser.curr.kind])

proc fromJsonL*(str: string): JsonNode = 
  ## Parse line-delimited JSON from `str` and returns a `JsonNode` array
  var parser = Parser(lexer: newLexer(str))
  parser.curr = parser.nextToken()
  parser.next = parser.nextToken()
  result = newJArray()
  while parser.curr.kind != tkEof:
    case parser.curr.kind
    of tkLBrace:
      var obj = newJObject()
      parser.parseObject(obj)
      result.add(obj)
    of tkLBracket:
      var arr = newJArray()
      parser.parseArray(arr)
      result.add(arr)
    else:
      echo parser.curr
      parser.error(unexpectedToken % [$parser.curr.kind])

#
# Nim Objects
#
proc parseJson(parser: var Parser, v: var object) =
  case parser.curr.kind
  of tkLBrace:
    parser.parseHook("", v)
  else:
    parser.error(unexpectedToken % [$parser.curr.kind])

macro fromJsonMacro(x: typed, str: typed): untyped =
  # macro to parse JSON string `str` into object of type `x`
  var objIdent = x.getTypeImpl()[1]
  # var objRef: bool
  var
    blockStmtList = newStmtList()
    blockStmtId = genSym(nskLabel, "voodoo")
  add blockStmtList, quote do:
    var
      tmp = `objIdent`()
      parser = Parser(lexer: newLexer(`str`))
    parser.curr = parser.nextToken()
    parser.next = parser.nextToken()
    parser.parseJson(tmp)
    ensureMove(tmp) # return the parsed object
  var blockStmt = newBlockStmt(blockStmtId, blockStmtList)
  result = newStmtList().add(blockStmt)

proc fromJson*[T](s: string, x: typedesc[T]): T =
  ## Provide a direct to object conversion from JSON string to Nim objects
  when x is JsonNode:
    return fromJson(s)
  else:
    return fromJsonMacro(x, s)
