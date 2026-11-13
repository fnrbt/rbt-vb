# Changelog

## 0.2.0 — 2026-11-13

### Compatibility

- Strict and tolerant parsing must consume the full source; malformed trailing input no longer executes a successful prefix.
- Restore VB operator precedence, logical-line continuation, inline conditional statement lists, argumentless procedure calls, and unconditional Do loops.
- Resolve callable declarations before compiling bodies and isolate class/member scope from ordinary variables.
- Correct ElseIf chains, runtime For steps, and nested/global For Each state.
- Preserve method ByRef arguments, distinguish Property Let/Set, and resolve script members case-insensitively. Host member spelling remains unchanged.
- Make error handlers procedure-local and recover at statement boundaries. Explicit Err.Raise uses the same dispatch as runtime failures.
- Preserve multidimensional array bounds/indexing and ReDim Preserve semantics; invalid indices and shapes report errors.
- Share numeric conversion and comparison semantics between VM operations and builtins, including ties-to-even rounding, string ordering, and StrComp/InStr comparison arguments.
- Validate persisted and directly supplied bytecode before execution. The updated bytecode format rejects incompatible versions rather than guessing a migration.

These changes intentionally replace previously silent truncation, ignored calls/assignments, and incorrect numeric results with correct execution or explicit errors. AST/value/bytecode consumers must migrate with the new contracts; compatibility aliases are not retained.

### Packaging

- Publish builds disable tiered JIT in the F# compiler host to avoid an observed .NET 10 CLR crash when compiling the portable Release target. Library Release optimization remains enabled.

### Verification

Run the native assertion suite:

```sh
dotnet run --project tests/Rbt.Vb.Tests.fsproj -c Release
```

Run the shared suite through the pinned Fable tool, emitting JavaScript outside the source tree:

```sh
dotnet tool restore
dotnet fable tests/Rbt.Vb.Tests.fsproj --outDir /tmp/rbt-vb-tests --extension .mjs
node /tmp/rbt-vb-tests/Program.mjs
```

Persistence tests are native-only because persisted bytecode is not supported by the Fable runtime. Shared parser/compiler/VM/builtin regressions run on both surfaces. Publishing executes the native suite before packing or uploading; ordinary Forge changes select the test action.

### Source layout

`ParserSimple.fs` and `StackVm.fs` are the shipping implementations. `Parser.fs` and `Vm.fs` are excluded historical implementations and are not maintained as compatibility alternatives.
