# Working with Nim's macros is just Voodoo
#
# (c) 2024 George Lemon | MIT License
#          Made by Humans from OpenPeeps
#          https://github.com/openpeeps/voodoo

import std/[macros, tables, strutils, sequtils]

var
  genSetters {.compileTime.} = OrderedTable[string, NimNode]()
  genGetters {.compileTime.} = OrderedTable[string, NimNode]()
  excludeFields {.compiletime.}: seq[string]

proc genProcIdent(x: NimNode): NimNode {.compileTime.} =
  let fieldName = $(x)
  var procName = "get"
  var i: int
  add procName, fieldName[i].toUpperAscii
  inc i
  while i < fieldName.len:
    case fieldName[i]
    of '_':
      while fieldName[i] == '_':
        inc i
      add procName, fieldName[i].toUpperAscii
      inc i
    else:
      add procName, fieldName[i]
      inc i
  result = ident(procName)

#
# Setter
#
macro setter*(obj: untyped) =
  ## Generate setter procs from object fields
  discard # todo

macro setter*(excludes: untyped, obj: untyped) =
  ## Optionally, you can exclude fields from generation
  expectKind excludes, nnkBracket
  excludeFields = excludes.mapIt($it)
  echo obj[0].treeRepr
  add obj[0][1], ident("setters")
  obj

macro expandSetters* =
  ## Required to inject generated setters in your code
  result = newStmtList()
  for k, x in genSetters:
    add result, x
  clear(genSetters)

#
# Getter
#
macro getters*(obj: untyped) =
  ## Generate getter procs from object fields
  let objident = obj[0][0][1]
  let objf = obj[2][0][2]
  obj[0] = obj[0][0]
  expectKind objf, nnkRecList
  genGetters[objident.strVal] = newStmtList()
  for f in objf:
    var procName: NimNode
    var returnType = f[^2]
    var fieldName: string
    for x in f[0..^3]:
      case x.kind
      of nnkIdent, nnkAccQuoted:
        if excludeFields.contains($x):
          continue
        fieldName = $x
        procName = genProcIdent(x)
      else: discard
      var body = newStmtList()
      add body, newCommentStmtNode("Getter handle to return `" & $fieldName & "`")
      add genGetters[objident.strVal],
        newProc(
          nnkPostfix.newTree(ident("*"), procName),
          params = [
            returnType,
            nnkIdentDefs.newTree(
              ident(objident.strVal[0].toLowerAscii & objident.strVal[1..^1]),
              objident,
              newEmptyNode()
            ),
          ],
          body = body
        )
  obj

macro getters*(excludes: untyped, obj: untyped) =
  ## Optionally, you can exclude fields from generation
  expectKind excludes, nnkBracket
  excludeFields = excludes.mapIt($it)
  echo obj[0].treeRepr
  add obj[0][1], ident("getters")
  obj

macro expandGetters* =
  ## Required to inject the generated getters in your code
  result = newStmtList()
  for k, x in genGetters:
    add result, x
  clear(genGetters)
