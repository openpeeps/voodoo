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
    of nnkPostfix:
      fieldName = $x[1]
      procName = genProcIdent(x[1])
    else: discard # error
    let paramIdent = ident(id.strVal[0].toLowerAscii & id.strVal[1..^1])
    var body = newStmtList()
    add body, newCommentStmtNode("Get `" & $fieldName & "` from `" & $id & "`")
    add body, 
      newAssignment(
        ident"result",
        newDotExpr(paramIdent, ident(fieldName))
      )
    add genGetters[id.strVal],
      newProc(
        nnkPostfix.newTree(ident("*"), procName),
        params = [
          returnType,
          nnkIdentDefs.newTree(
            paramIdent,
            id,
            newEmptyNode()
          ),
        ],
        body = body
      )

macro getters*(obj: untyped) =
  var objident: NimNode
  case obj[0][0].kind
    of nnkPostfix:
      objident = obj[0][0][1]
    of nnkIdent:
      objIdent = obj[0][0]
    else: discard # error?
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

macro expandGetters* =
  ## This is required to insert generated getters
  result = newStmtList()
  for k, x in genGetters:
    add result, x
  clear(genGetters)

macro expandGetters*(identRenameCallback: static proc(x: string): string) =
  ## Expand generated getter procedures. Use `identRenameCallback`
  ## to refine proc identifiers 
  result = newStmtList()
  for k, x in genGetters:
    if identRenameCallback != nil:
      for p in x:
        p[0][1] = ident(identRenameCallback(p[0][1].strVal))
    add result, x
  clear(genGetters)
