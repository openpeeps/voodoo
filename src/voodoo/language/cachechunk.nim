import std/[tables]
import pkg/flatty

import ./chunk

const
  BytecodeCacheMagic* = "VOODOOBC"
  BytecodeCacheVersion* = 1
  OpcodeVersion* = 1 # bump whenever opcode encoding changes

type
  CachedHeader* = object
    magic*: string
    cacheVersion*: int
    opcodeVersion*: int
    sourceHash*: string

  CachedChunk* = object
    file*: string
    code*: seq[uint8]
    ln*: int
    col*: int
    lineInfo*: seq[LineInfo]
    strings*: seq[string]

  CachedProc* = object
    name*: string
    kind*: ProcKind
    paramCount*: int
    hasResult*: bool
    chunkIndex*: int # only used for pkNative

  CachedScript* = object
    header*: CachedHeader
    mainChunkIndex*: int
    chunks*: seq[CachedChunk]
    procs*: seq[CachedProc]

proc toCachedChunk(c: Chunk): CachedChunk =
  result.file = c.file
  result.code = c.code
  result.ln = c.ln
  result.col = c.col
  result.lineInfo = c.getLineInfoTable()
  result.strings = c.strings

proc fromCachedChunk(cc: CachedChunk): Chunk =
  result = newChunk(cc.file)
  result.code = cc.code
  result.ln = cc.ln
  result.col = cc.col
  result.strings = cc.strings
  result.setLineInfoTable(cc.lineInfo)
  result.rebuildStringIds()

proc toCachedScript*(s: Script, sourceHash: string): CachedScript =
  var chunkToIndex = initTable[Chunk, int]()
  var chunks: seq[CachedChunk]

  proc internChunk(c: Chunk): int =
    if c in chunkToIndex: return chunkToIndex[c]
    let idx = chunks.len
    chunkToIndex[c] = idx
    chunks.add(toCachedChunk(c))
    idx

  let mainIdx = internChunk(s.mainChunk)

  var cps: seq[CachedProc]
  for p in s.procs:
    case p.kind
    of pkNative:
      cps.add(CachedProc(
        name: p.name,
        kind: p.kind,
        paramCount: p.paramCount,
        hasResult: p.hasResult,
        chunkIndex: internChunk(p.chunk)
      ))
    of pkForeign:
      cps.add(CachedProc(
        name: p.name,
        kind: p.kind,
        paramCount: p.paramCount,
        hasResult: p.hasResult,
        chunkIndex: -1
      ))

  result = CachedScript(
    header: CachedHeader(
      magic: BytecodeCacheMagic,
      cacheVersion: BytecodeCacheVersion,
      opcodeVersion: OpcodeVersion,
      sourceHash: sourceHash
    ),
    mainChunkIndex: mainIdx,
    chunks: chunks,
    procs: cps
  )

proc fromCachedScript*(cs: CachedScript, expectedSourceHash: string): Script =
  if cs.header.magic != BytecodeCacheMagic:
    raise newException(ValueError, "invalid bytecode cache magic")
  if cs.header.cacheVersion != BytecodeCacheVersion:
    raise newException(ValueError, "bytecode cache version mismatch")
  if cs.header.opcodeVersion != OpcodeVersion:
    raise newException(ValueError, "opcode version mismatch")
  if cs.header.sourceHash != expectedSourceHash:
    raise newException(ValueError, "source hash mismatch")

  var chunks: seq[Chunk]
  for cc in cs.chunks:
    chunks.add(fromCachedChunk(cc))

  result = newScript(chunks[cs.mainChunkIndex])
  result.libs = initTable[string, LibHandle]()
  result.scripts = initTable[string, Script]()

  for cp in cs.procs:
    case cp.kind
    of pkNative:
      result.procs.add(Proc(
        name: cp.name,
        kind: pkNative,
        chunk: chunks[cp.chunkIndex],
        paramCount: cp.paramCount,
        hasResult: cp.hasResult
      ))
    of pkForeign:
      result.procs.add(Proc(
        name: cp.name,
        kind: pkForeign,
        foreign: nil, # rebind after load
        paramCount: cp.paramCount,
        hasResult: cp.hasResult
      ))

proc saveCachedScript*(path: string, cs: CachedScript) =
  ## Saves a cached script to the given path. This will
  ## overwrite any existing file at that path.
  writeFile(path, toFlatty(cs))

proc loadCachedScript*(path: string): CachedScript =
  ## Loads a cached script from the given path. Raises an
  ## exception if the file is not a valid cache.
  fromFlatty(readFile(path), CachedScript)

proc tryLoadCachedScript*(path: string, expectedSourceHash: string, outScript: var Script): bool =
  ## Returns false on any cache miss/invalid blob/version mismatch.
  if not fileExists(path):
    return false
  try:
    let cs = loadCachedScript(path)
    outScript = fromCachedScript(cs, expectedSourceHash)
    return true
  except CatchableError:
    return false

proc saveCachedScriptAtomic*(path: string, cs: CachedScript) =
  ## Avoid partial/corrupt cache files on crash.
  let tmp = path & ".tmp"
  writeFile(tmp, toFlatty(cs))
  moveFile(tmp, path)