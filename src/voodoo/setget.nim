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
macro setters*(obj: untyped) =
  ## Generate setter procs from object fields
  discard # todo

macro setters*(excludes: untyped, obj: untyped) =
  ## Optionally, you can exclude fields from generation
  expectKind excludes, nnkBracket
  excludeFields = excludes.mapIt($it)
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
proc walkField(f: NimNode, id: NimNode) {.compileTime.} =
  var
    procName: NimNode
    returnType = f[^2]
    fieldName: string
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
    add genGetters[id.strVal],
      newProc(
        nnkPostfix.newTree(ident("*"), procName),
        params = [
          returnType,
          nnkIdentDefs.newTree(
            ident(id.strVal[0].toLowerAscii & id.strVal[1..^1]),
            id,
            newEmptyNode()
          ),
        ],
        body = body
      )

macro getters*(obj: untyped) =
  let objident = obj[0][0][1]
  var objf: NimNode
  if obj[2].kind == nnkRefTy:
    objf = obj[2][0][2]
  else:
    objf = obj[2][2]
  obj[0] = obj[0][0]
  expectKind objf, nnkRecList
  genGetters[objident.strVal] = newStmtList()
  for f in objf:
    case f.kind
    of nnkRecCase:
      for ff in f[1..^1]:
        expectKind(ff[1], nnkRecList)
        ff[1][0].walkField(objident)
    else:
      f.walkField(objident)
  obj

macro getters*(excludes: untyped, obj: untyped) =
  expectKind excludes, nnkBracket
  excludeFields = excludes.mapIt($it)
  add obj[0][1], ident("getters")
  obj

macro expandGetters*(identRenameCallback: static proc(x: string): string = nil) =
  ## This is required to insert generated getters
  ## Use `identRename` to rename  
  result = newStmtList()
  for k, x in genGetters:
    if identRenameCallback != nil:
      for p in x:
        p[0][1] = ident(identRenameCallback(p[0][1].strVal))
    add result, x
  clear(genGetters)
