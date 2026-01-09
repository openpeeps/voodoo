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
  if ExtendableEnums.hasKey($x):
    for xfield in otherFields:
      add ExtendableEnums[$x], xfield
  else:
    ExtendableEnums[$x] = otherFields

macro extendProc*(x: untyped) =
  ## Extend a module by adding custom procedures
  let moduleSource = instantiationInfo(fullPaths = true).filename
  if ExtendableProcs.hasKey(moduleSource):
    var existingProcs = ExtendableProcs[moduleSource]
    if x.kind == nnkStmtList:
      for procNode in x:
        add existingProcs, procNode
    else:
      add existingProcs, x
    ExtendableProcs[moduleSource] = existingProcs
  else:
    if x.kind == nnkStmtList:
      ExtendableProcs[moduleSource] = x
    else:
      var newProcs = newStmtList()
      add newProcs, x
      ExtendableProcs[moduleSource] = newProcs

macro extendCase*(struct: untyped) =
  ## Extend an object variant by adding new branches at compile time.
  expectKind(struct[0], nnkTypeSection)
  expectKind(struct[0][0], nnkTypeDef)
  let objDef = struct[0][0]
  let objName = objDef[0]
  var caseFieldName: NimNode
  # todo handle pragmas?
  var objCases: seq[NimNode]
  if objDef[2].kind == nnkRefTy:
    # objDef[2] = objDef[2][0]
    expectKind(objDef[2][0][2], nnkRecList)
    # the first case, which is the case we want to extend
    expectKind(objDef[2][0][2][0], nnkRecCase)
    let recCase = objDef[2][0][2][0]
    caseFieldName = recCase[0][0] # ident
    objCases = objDef[2][0][2][0][1..^1]
  else:
    expectKind(objDef[2], nnkObjectTy)
    # the first case, which is the case we want to extend
  Extendables[$objName & "_" & $caseFieldName] = newStmtList().add(objCases)

template extendCase2*(fieldNode: untyped, branchesNode: untyped) =
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
      echo br.treeRepr
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

template extensibleCase* {.pragma.}

macro extensible*(x: untyped) =
  ## Mark your object or enum with `extensible` pragma
  ## for making it extensible from other modules
  expectKind(x, nnkTypeDef)
  let objName =
    if x[0][0].kind == nnkPostfix:
      x[0][0][1]
    else:
      x[0][0]
  if x[2].kind in {nnkObjectTy, nnkRefTy}:
    let obj = if x[2].kind == nnkRefTy: x[2][0][2] else: x[2][2]
    for objNode in obj:
      case objNode.kind
      of nnkRecCase:
        var isExtensible: bool
        if objNode[0][0].kind == nnkPragmaExpr:
          for somePragma in objNode[0][0][^1]:
            if somePragma.eqIdent"extensibleCase":
              isExtensible = true
              break
        let fieldName =
          if objNode[0][0][0].kind == nnkAccQuoted:
            objNode[0][0][0]
          elif objNode[0][0].kind == nnkPragmaExpr:
            objNode[0][0][0][1]
          else:
            objNode[0][0]
        if isExtensible:
          # checking if the case variant is marked as extensible
          let key = $objName & "_" & $fieldName
          if Extendables.hasKey(key):
            for br in Extendables[key]:
              if x[2].kind == nnkRefTy:
                insert(x[2][0][2][1], x[2][0][2][1].len - 1, br)
              else:
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
