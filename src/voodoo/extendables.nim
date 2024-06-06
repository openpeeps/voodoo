# Working with Nim's macros is just Voodoo
#
# (c) 2024 George Lemon | MIT License
#          Made by Humans from OpenPeeps
#          https://github.com/openpeeps/voodoo

import std/[macros, macrocache]
export macros

const
  Extendables* = CacheTable"Extendables"
  ExtendableEnums* = CacheTable"ExtendableEnums"
  ExtendableProcs* = CacheTable"ExtendableProcs"

macro extendEnum*(x: untyped, fields: untyped) =
  ## Extend a specific enum by adding extra fields
  expectKind(x, nnkIdent)
  expectKind(fields, nnkStmtList)
  var otherFields = newStmtList()
  for f in fields:
    case f.kind 
    of nnkAsgn:
      add otherFields, nnkEnumFieldDef.newTree(f[0], f[1])
    of nnkIdent:
      add otherFields, f
    else:
      error("Voodoo - Invalid enum extension. Expects either `nnkAsgn`, or `nnkIdent`")
  ExtendableEnums[$x] = otherFields

template extendCase*(fieldNode: untyped, branchesNode: untyped) =
  ## Extend an object variant by adding new branches.
  ## This macro can be mixed with `extendEnum
  macro extendCaseMacro(x, branches) =
    expectKind(x, nnkDotExpr)
    let
      objName = x[0]
      fieldName = x[1]
      moduleSource = instantiationInfo(fullPaths = true).filename
    expectKind(branches, nnkStmtList)
    var stmtBranches = newStmtList()
    for br in branches:
      if not br[0].eqIdent "branch":
        error("Voodoo - Invalid branch extension. Expected a `branch` command")
      let branchType = br[1]
      expectKind(br, nnkCommand)
      expectKind(br[1], nnkIdent)
      expectKind(br[2], nnkStmtList)
      var branch = newNimNode(nnkRecList)
      for f in br[2]:
        var fieldName: NimNode 
        var implValue: NimNode
        expectKind(f, nnkCall)
        expectKind(f[1], nnkStmtList)
        if f[1][0].kind == nnkAsgn:
          fieldName = f[1][0][0]
          implValue = f[1][0][1]
        else:
          fieldName = f[1][0]
          implValue = newEmptyNode()
        add branch,
          nnkIdentDefs.newTree(
            f[0], fieldName, implValue
          )
      add stmtBranches, nnkOfBranch.newTree(branchType, branch)
      if br[^1].kind == nnkStmtList:
        ExtendableProcs[moduleSource] = br[^1]
    Extendables[$objName & "_" & $fieldName] = stmtBranches
  extendCaseMacro(fieldNode, branchesNode)

template extendableCase* {.pragma.}

macro extendable*(x: untyped) =
  ## Mark your object or enum with `extendable` pragma
  ## for making it extendable from other modules at compile-time.
  expectKind(x, nnkTypeDef)
  let objName =
    if x[0][0].kind == nnkPostfix:
      x[0][0][1]
    else:
      x[0][0]
  if x[2].kind == nnkObjectTy:
    for objNode in x[2][2]:
      case objNode.kind
      of nnkRecCase:
        var isExtendable: bool
        if objNode[0][0].kind == nnkPragmaExpr:
          for somePragma in objNode[0][0][^1]:
            if somePragma.eqIdent"extendableCase":
              isExtendable = true
              break
        let fieldName =
          if objNode[0][0][0].kind == nnkAccQuoted:
            objNode[0][0][0]
          else:
            objNode[0][0]
        if isExtendable:
          let key = $objName & "_" & $fieldName
          if Extendables.hasKey(key):
            for br in Extendables[key]:
              insert(x[2][2][1], x[2][2][1].len - 1, br)
      else: discard
  elif x[2].kind == nnkEnumTy:
    if ExtendableEnums.hasKey(objName.strVal):
      for enumField in ExtendableEnums[objName.strVal]:
        add x[2], enumField
  x

template injectHandles* =
  ## Injects custom procedures and other handles.
  ## This macro should be called in the source of the
  ## original module.
  macro expandHandles =
    let moduleName = instantiationInfo(fullPaths = true).filename
    if likely(ExtendableProcs.hasKey(moduleName)):
      result = ExtendableProcs[moduleName]
  expandHandles()
