# Working with Nim's macros is just Voodoo
#
# (c) 2024 George Lemon | MIT License
#          Made by Humans from OpenPeeps
#          https://github.com/openpeeps/voodoo

import std/[macros, macrocache]
export macros, macrocache

const
  Extendables* = CacheTable"Extendables"
  ExtendableEnums* = CacheTable"ExtendableEnums"
  ExtendableProcs* = CacheTable"ExtendableProcs"
  ExtendableProcBodies* = CacheTable"ExtendableProcBodies"
  ExtendableModules* = CacheTable"ExtendableModules"
  ExtendableCases* = CacheTable"ExtendableCases"

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

macro extendModule*(modulePath: static string, x: untyped) =
  ## Extend a module by adding custom procedures
  # let moduleSource = instantiationInfo(fullPaths = true).filename
  if ExtendableModules.hasKey(modulePath):
    var existingProcs = ExtendableModules[modulePath]
    if x.kind == nnkStmtList:
      for procNode in x:
        add existingProcs, procNode
    else:
      add existingProcs, x
    ExtendableModules[modulePath] = existingProcs
  else:
    if x.kind == nnkStmtList:
      ExtendableModules[modulePath] = x
    else:
      var newProcs = newStmtList()
      add newProcs, x
      ExtendableModules[modulePath] = newProcs

macro extendCaseStmt*(id: static string, caseStmt: untyped) =
  ## Extend an `case` statement by adding new branches at compile time.
  expectKind(caseStmt, nnkStmtList)
  expectKind(caseStmt[0], nnkCaseStmt)
  ExtendableCases[id] = caseStmt

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

template extendableCase*(caseId: static string, caseStmtNode: untyped) =
  ## Extend an object variant by adding new branches at compile time.
  macro extendableCaseMacro(id: static string, caseStmt) =
    let caseSourcePath = instantiationInfo(fullPaths = true).filename
    expectKind(caseStmt, nnkStmtList)
    expectKind(caseStmt[0], nnkCaseStmt)
    for extendableCasePath, extendableCase in ExtendableCases:
      if ExtendableCases.hasKey(id):
        let extendedBranch = ExtendableCases[id]
        expectKind(extendedBranch, nnkStmtList)
        expectKind(extendedBranch[0], nnkCaseStmt)
        for newBranch in extendedBranch[0][1..^1]: # skip the case expression
          caseStmt[0].insert(caseStmt[0].len - 1, newBranch) # before the `else`
      break
    result = caseStmt
  extendableCaseMacro(caseId, caseStmtNode)

template placeholderSnippet*(snippetId: static string) =
  ## A placeholder for injecting custom code into a proc or other code callback
  ## based on the identifier of the placeholder.
  macro placeholderSnippetMacro(id: static string) =
    let snippetSourcePath = instantiationInfo(fullPaths = true).filename
    if ExtendableProcBodies.hasKey(id):
      result = nnkBlockStmt.newTree(
        ident("VoodooInjectedSnippet_" & id),
        ExtendableProcBodies[id]
      )
  placeholderSnippetMacro(snippetId)

macro injectSnippet*(id: static string, stmt: untyped): untyped =
  ## Injects a custom snippet of code into a proc or other code callback
  ## based on the identifier of the placeholder.
  if ExtendableProcBodies.hasKey(id):
    var existingProcs = ExtendableProcBodies[id]
    if stmt.kind == nnkStmtList:
      for procNode in stmt:
        add existingProcs, procNode
    else:
      add existingProcs, stmt
    ExtendableProcBodies[id] = existingProcs
  else:
    if stmt.kind == nnkStmtList:
      ExtendableProcBodies[id] = stmt
    else:
      ExtendableProcBodies[id] = newStmtList().add(stmt)

template injectHandles* =
  ## Injects custom procedures and other handles.
  ## This macro should be called in the source of the
  ## original module.
  macro expandHandles =
    let moduleName = instantiationInfo(fullPaths = true).filename
    for modulePath, procNode in ExtendableProcs:
      if moduleName.endsWith(modulePath) and ExtendableProcs.hasKey(modulePath):
        result = ExtendableProcs[modulePath]
        break
  expandHandles()

template injectExtendedModule* = 
  ## Inject custom code into the module.
  macro expandHandles =
    let moduleName = instantiationInfo(fullPaths = true).filename
    for modulePath, nimNode in ExtendableModules:
      if moduleName.endsWith(modulePath) and ExtendableModules.hasKey(modulePath):
        result = nimNode
        break
  expandHandles()