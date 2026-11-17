# Changelog

## 0.3.0 — 2026-11-17

This release fixes VB programs that ran but produced wrong results. It also adds VB error numbers and the host API for macros and an in-app VBA IDE. See [docs/embedding.md](docs/embedding.md).

### Silent wrong results fixed

- `Enum` members (implicit and explicit values, `Enum.Member`) no longer evaluate to Empty.
- `Type` (UDT) fields store values. UDT variables, arrays of UDTs and nested UDTs are initialised, and assigning a UDT copies it.
- `Optional` defaults are applied. `IsMissing` works for Variant optionals, typed optionals start at their type default, and `ParamArray` is supported.
- Omitted arguments (`F(1, , 3)`, `Address(, False)`) are supported. Named arguments (`F(b:=2, a:=1)`, `ws.Find What:="x"`) are reordered for VB procedures, mapped for MsgBox, InputBox and a few other built-ins, and passed to hosts through `CallNamed`.
- Literals: `&HFF`, `&O17`, `&HFFFF` (Integer −1), `&HFFFF&` (Long), `1E3` and `2.5E-1`, and the type suffixes `5&`, `5%`, `5#`, `5!`, `5@`. `Left$`-style names are accepted.
- Nested `With` blocks, and nested `Select Case` blocks, each get their own slot.
- Indexing a call result works: `Split(s, ",")(1)`, `F()(i)`.
- The `vb*` constants are built in: strings, compare modes, `vbObjectError`, VarType, MsgBox buttons/icons/results, colours and days of the week.
- Arguments are evaluated left to right.
- A recursive call passed to a ByRef parameter (`Ack(m - 1, Ack(m, n - 1))`) no longer fails with "Expected array".
- `Debug.Print` follows VBA formatting: `;` and `,` separators, numbers with a sign space.
- `Concat` with Null yields the other operand.
- `And`, `Or`, `Xor` and `Not` are bitwise on integers.
- `^` always returns Double.
- `RGB` and `QBColor` are new built-ins.

### Error handling

- New statements: `Resume`, `Resume Next`, `Resume <label>` and `On Error GoTo -1`. `On Error`, `Resume` and leaving a procedure from its handler clear `Err`.
- Errors carry their VB numbers and standard descriptions: 3, 5, 6, 7, 9, 11, 13, 18, 20, 28, 35, 91, 94, 424, 429, 438, 440, 448, 449, 450 and 451. Calls with too many or missing arguments raise 450 and 449. Member access on Nothing/Empty raises 91 and on non-objects 424, where it used to be silently ignored.
- `Err` is an object: `TypeName(Err)` is `"ErrObject"`, `Err` reads `Err.Number`, and `Err.Number`, `Err.Description` and `Err.Source` can be assigned. `Err.Raise number, source, description` sets `Err.Source`, which defaults to the project name. `Erl` returns 0.
- Every instruction has a source line. Unhandled errors report their number, description, line, procedure and module.

### Declared types

- `Dim x As Integer/Long/Single/Double/Currency/String/Boolean/Byte/Date/Object` starts at 0, `""`, False or Nothing, as do typed fields, typed Function results and arrays such as `Dim a(9) As Long`.
- Assigning to a typed variable converts the value. Integer, Long and Byte round to even (`i = 7.6` gives 8) and raise error 6 when out of range.
- Untyped integer arithmetic that leaves the 32-bit range is promoted to Double instead of wrapping.
- `TypeName` and `VarType` report Long for integers outside the 16-bit range.
- Currency, Decimal and Date are stored as Double for now.
- `Option Base 1` is honoured.
- `ReDim a(n) As T` is accepted. The element type comes from the variable's `Dim`.

### Host bridge (breaking for `IHostObject` implementers)

- `IHostObject` has two new members:
  - `CallNamed name args names` handles calls with named arguments. Return `this.CallMethod name args` to ignore the names.
  - `Enumerate ()` backs `For Each`. Return `None` if the object is not a collection.
- New `HostObject(typeName)` base class with overridable defaults.
- Unknown members (`None`/`false`) now raise error 438 instead of reading as Empty or being ignored.
- Default members:
  - `Range("A1") = 5`, `Cells(r, c) = x` and `ws.Range("B1") = 9` assign through `SetMember ""` on the object returned by `CallMethod "" args`.
  - Value contexts read through `GetMember ""`, then `"_Default"`, then `"Value"`. This covers `x = Range("A1")`, arithmetic, comparisons, `&`, built-in arguments and `MsgBox`. `Set` still binds the object.
- A host call or a host global's default call passed as an argument to a host method now works: `Range("A1:B1").Copy Range("D1")`.
- Hosts raise VB errors with `raiseVbError n` / `VbError { Number; Description; Source }`, and `On Error` sees the number. Other host exceptions become error 440.
- Omitted arguments reach hosts as `VUndefined`.
- Hosts can provide constants as plain-value host globals.

### Embedding API

- `Project.compile` compiles `(name, Standard | Class | Document, source)` modules together:
  - `VERSION`/`BEGIN`/`Attribute` header lines are blanked, so line numbers are kept, and `ModuleSource.parseHeader` reads them.
  - Procedures are addressable as `Module.Proc`, and public procedures of standard modules also by bare name.
  - Public module variables are shared; Private ones are per module.
  - Class modules are classes.
- `Project.procedures` and `Project.macros` return each procedure's name, module, kind, parameters (with literal defaults), visibility and start/end lines.
- `Session` holds persistent module state:
  - `Call(name, args)` runs a named procedure.
  - `Evaluate`, `Execute` and `Immediate` (`?expr`, `Print expr`, statements) work in an optional module scope. Undeclared Immediate variables persist.
  - Execution is cooperative: `Step maxInstructions` returns `Running | Finished value | Failed failure`, with `RunToEnd`, `Cancel` (error 18) and `Location` while paused.
- `RuntimeHooks` provide `Debug.Print`/`Print` output, `MsgBox`, `InputBox` and `CreateObject`. Pass them through `RunOptions.withHooks` or `ProjectOptions.withHooks`. `defaultHooks` keeps the console behaviour, and `CreateObject` defaults to error 429.
- New structured `VbFailure { Kind; Number; Description; Source; Line; Column; Procedure; Module }` for syntax, compile and run-time errors.
- `RunOptions` gains `Hooks` and `InstructionLimit`. `executeWith` runs bytecode with hooks and an instruction limit.
- `execute`/`executeWithHostGlobals` raise `StackVm.VbRuntimeException`, whose `Message` is formatted and whose `.Failure` holds the details.
- `runSource` reports `RuntimeError "Run-time error '11': Division by zero (line 4 in Inner)"`.

### Migrating from 0.1.2/0.2.0 (hosts)

1. Add the two new members to each `IHostObject` implementation:
   ```fsharp
   member this.CallNamed name args _ = (this :> IHostObject).CallMethod name args
   member _.Enumerate () = None
   ```
   Alternatively, inherit `HostObject`.
2. Return `None` only for members that really do not exist. Return `Some VEmpty` for members that exist but have no value, because `None` now raises error 438.
3. Map the default member: handle `GetMember ""` and `SetMember ""` (for example as `Value`), so that `Range("A1") = 5` and `x = Range("A1")` work. Reads also fall back to `"Value"`.
4. Raise `Values.VbError` (or `raiseVbError 1004`) for errors scripts should be able to trap by number.
5. Optional: provide MsgBox, InputBox, Debug.Print and CreateObject through `RuntimeHooks` instead of host globals, and use `Project`/`Session` for macro lists, named macro runs, the Immediate window and Stop.
6. Code that relied on silent wrong results changes behaviour: wrapping Long arithmetic (now error 6 for typed variables, Double for Variants), member access on Nothing (now error 91), and unknown members (now error 438).

### Bytecode

Format version 3 adds:
- opcodes for coercion, Let-coercion, For Each, Resume, Err, default assignment, UDT copy, array fill and named calls;
- module, parameter-name and initial-local metadata on functions;
- value-type and field-type metadata on classes;
- module start offsets.

Version 2 blobs are rejected.

### Benchmarks

The benchmarks' random-number generators multiplied a Long by 1103515245, which overflows and is error 6 in VBA. They now use `seed * 25173 + 13849`. Timings are the minimum of 7 interleaved runs of the 0.2.0 CLI and the 0.3.0 CLI on the same updated scripts, pinned to two cores, on a shared machine with load around 6. The noise is about ±10%.

| Benchmark | 0.2.0 | 0.3.0 |
| --- | ---: | ---: |
| bench_matrix (50×50 multiply) | 0.334 s | 0.320 s |
| bench_objects (500 particles × 100 steps) | 0.274 s | 0.276–0.320 s |
| bench_recursive Fib(28) | 0.409 s | 0.413–0.426 s |
| bench_recursive Ack(3,7) | fails ("Expected array") | 0.211 s |
| bench_recursive Hanoi(20) | not reached | 0.680 s |
| bench_recursive QuickSort 5000 | not reached | 0.061–0.076 s |
| bench_sieve (100000) | 0.358 s | 0.369 s |
| bench_strings | 0.037 s | 0.036 s |

Typed variables add a conversion on each store, which is what Long overflow detection costs: about 4% on Fib.

### Verification

- Native: `dotnet run --project tests/Rbt.Vb.Tests.fsproj -c Release` passes 60 public-API groups, the value-semantics suite, 45 new 0.3.0 release groups, persistence and the fixtures.
- Fable 5.27.0 / Node: the same shared suites pass, including all 45 release groups.

### Not in this release

The following are planned for 0.4.0:
- Date and Format functions;
- Collection and Dictionary;
- more built-ins;
- class-module events and `Implements`;
- `Static` variables;
- numeric line labels for `Erl`;
- full Currency/Decimal precision;
- `Dim x As New Class`;
- assignment through an object variable's default member (`r = 5` where `r` holds a host object);
- Option Explicit enforcement.

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
