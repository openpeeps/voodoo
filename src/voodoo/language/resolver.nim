# Voodoo - Prorgramming Language Framework for Nim
# So you can build your own programming language!
#
# (c) 2026 George Lemon | MIT License
#          Made by Humans from OpenPeeps
#          https://github.com/openpeeps/voodoo

import std/[os, tables, sequtils, sets]

type
  ResolvedFiles = Table[string, seq[string]]
  FileResolver* = object
    ## Manages file resolution for imports/includes
    resolvedFiles*: ResolvedFiles
      # Tracks which files have been resolved (imported/included)

  ResolverError* = object of CatchableError

proc initResolver*(): FileResolver =
  ## Initialize a new FileResolver
  result.resolvedFiles = ResolvedFiles()

proc fileExists*(resolver: FileResolver, filePath: string): bool =
  # Checks if the file exists on disk
  return fileExists(filePath)

proc isResolved*(resolver: FileResolver, filePath: string): bool =
  # Checks if the file has already been resolved (included/imported)
  result = resolver.resolvedFiles.hasKey(filePath)

proc ensureNode(resolver: var FileResolver, filePath: string) =
  if not resolver.resolvedFiles.hasKey(filePath):
    resolver.resolvedFiles[filePath] = @[]

proc dependencies*(resolver: FileResolver, filePath: string): seq[string] =
  resolver.resolvedFiles.getOrDefault(filePath, @[])

proc setDependencies*(resolver: var FileResolver, aFile: string, deps: openArray[string]) =
  resolver.ensureNode(aFile)
  var normalized: seq[string] = @[]
  for dep in deps:
    let d = normalizedPath(dep)
    if d != aFile and d notin normalized:
      normalized.add(d)
      resolver.ensureNode(d)
  resolver.resolvedFiles[aFile] = normalized

proc clearFile*(resolver: var FileResolver, filePath: string) =
  for key in resolver.resolvedFiles.keys.toSeq:
    resolver.resolvedFiles[key] = resolver.resolvedFiles[key].filterIt(it != filePath)
  if resolver.resolvedFiles.hasKey(filePath):
    resolver.resolvedFiles.del(filePath)

proc dependants*(resolver: FileResolver, target: string, recursive = true): seq[string] =
  if not recursive:
    for file, deps in resolver.resolvedFiles:
      if target in deps:
        result.add(file)
    return

  var
    visited = initHashSet[string]()
    stack = @[target]

  while stack.len > 0:
    let current = stack.pop()
    for file, deps in resolver.resolvedFiles:
      if current in deps and file notin visited:
        visited.incl(file)
        result.add(file)
        stack.add(file)

# proc markResolved(resolver: var FileResolver, aFile, bFile: string) =
#   # Marks a file as resolved (included/imported)
#   # `aFile`: the module doing the import
#   # `bFile`: the imported module
#   # Ensure both `aFile` and `bFile` have entries
#   if not resolver.resolvedFiles.hasKey(aFile):
#     resolver.resolvedFiles[aFile] = @[]

#   if not resolver.resolvedFiles.hasKey(bFile):
#     resolver.resolvedFiles[bFile] = @[]

#   # Add bFile to aFile's list, and aFile to bFile's list
#   if bFile notin resolver.resolvedFiles[aFile]:
#     resolver.resolvedFiles[aFile].add(bFile)

#   if aFile notin resolver.resolvedFiles[bFile]:
#     resolver.resolvedFiles[bFile].add(aFile)

proc resolveFile*(resolver: var FileResolver, aFile, bFile: string) =
  ## Resolve a file import/include. This proc checks
  ## if the file exists, if it has already been resolved,
  ## and if there are any circular or self-imports.
  ## 
  ## If all checks pass, it marks the file as resolved.
  ## TODO handle symlinks
  if not resolver.fileExists(bFile):
    raise newException(ResolverError, "File does not exist: " & bFile)
  if bFile == aFile:
    raise newException(ResolverError, "Self-import detected: " & aFile)

  resolver.ensureNode(aFile)
  resolver.ensureNode(bFile)

  if bFile notin resolver.resolvedFiles[aFile]:
    resolver.resolvedFiles[aFile].add(bFile)