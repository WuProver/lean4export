import Export
open Lean

def main (args : List String) : IO Unit := do
  initSearchPath (← findSysroot)
  let (opts, args) := args.partition (fun s => s.startsWith "--" && s.length ≥ 3)
  let (imports, constants) := args.span (· != "--")
  let imports := imports.toArray.map fun mod => { module := Syntax.decodeNameLit ("`" ++ mod) |>.get! }
  let env ← importModules imports {}
  let constants := match constants.tail? with
    | some cs => cs.map fun c => Syntax.decodeNameLit ("`" ++ c) |>.get!
    | none    => env.constants.toList.map Prod.fst |>.filter (!·.isInternal)
  M.run env do
    let _ ← initState env opts
    let dumpType? := opts.filterMap <|
      fun s ↦
        s.dropPrefix? "--dump-type=" |>.map
          fun s' ↦ Syntax.decodeNameLit ("`" ++ s'.toString) |>.get!
    assert! dumpType?.length < 2
    dumpMetadata
    match dumpType?[0]? with
    | none =>
      for c in constants do
        modify (fun st => { st with noMDataExprs := {} })
        dumpConstant c
    | some name => dumpType name
    for c in (← get).asAxioms.diff (← get).visitedConstants do
      IO.eprintln s!"warning: {c} isn't visited."
