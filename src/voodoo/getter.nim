# Working with Nim's macros is just Voodoo
#
# (c) 2024 George Lemon | MIT License
#          Made by Humans from OpenPeeps
#          https://github.com/openpeeps/voodoo

import std/[macros, macrocache, strutils]

const genGetters = CacheTable"genGetters"
macro getters*(obj: untyped) =
  # let impl = getImpl(x)
  let objident = obj[0][0][1]
  let objf = obj[2][0][2]
  obj[0] = obj[0][0]
  expectKind objf, nnkRecList
  proc genProcIdent(x: NimNode): NimNode =
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

  genGetters[objident.strVal] = newStmtList()
  for f in objf:
    var procName: NimNode
    var returnType = f[^2]
    var fieldName: string
    for x in f[0..^3]:
      case x.kind
      of nnkIdent, nnkAccQuoted:
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

macro expandGetters* =
  result = newStmtList()
  for k, x in genGetters:
    echo x.repr
    add result, x