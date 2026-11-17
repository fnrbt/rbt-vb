# Changelog

## 0.4.0 — 2026-11-17

This release adds the language features and built-ins that kept real Excel and Word macros from running: dates, Format, Collection and Dictionary, class events and interfaces, and the remaining statements. See [docs/embedding.md](docs/embedding.md) for the host-visible parts.

### Dates

- New `VDate` value: an OLE Automation date, days since 1899-12-30 with the time as the fraction. These are Excel's 1900-system serial numbers from 1900-03-01 on. `TypeName` is "Date" and `VarType` is 7.
- Date literals: `#2024-01-31#`, `#1/31/2024 13:45#`, `#1:45 PM#`.
- Arithmetic: Date + n and n + Date give a Date, Date − Date gives a Double, and Date − n gives a Date. Dates compare with Dates, numbers and date strings.
- `CDate`, `IsDate`, `DateValue`, `TimeValue`, `DateSerial` (months and days roll over; two-digit years 0–29 are 2000–2029), `TimeSerial`.
- `Date`, `Time`, `Now`, also without parentheses. `Date = ...` and `Time = ...` are accepted and ignored.
- `Year`, `Month`, `Day`, `Hour`, `Minute`, `Second`, `Weekday`, `WeekdayName`, `MonthName`.
- `DateAdd`, `DateDiff` and `DatePart` with "yyyy", "q", "m", "y", "d", "w", "ww", "h", "n" and "s". FirstDayOfWeek is honoured by all three; FirstWeekOfYear by `DatePart` and `Format` (`ww`).
- `Dim d As Date` holds a Date and converts strings and numbers assigned to it.
- Hosts: `Api.VbDate.ofParts / ofDateTime / ofSerial / tryToSerial / tryToParts / tryToDateTime`.

### Format, CStr, Str and Val

- `Format(value, format, firstDayOfWeek, firstWeekOfYear)`:
  - Named formats: General Number, Currency, Fixed, Standard, Percent, Scientific, Yes/No, True/False, On/Off, General Date, Long/Medium/Short Date and Long/Medium/Short Time.
  - Numeric formats: `0 # . ,` (grouping and ×1000 scaling), `%`, `E+ E- e+ e-`, literals, quoted text and `\` escapes, and up to four `;` sections.
  - Date formats: `c d dd ddd dddd ddddd dddddd w ww m mm mmm mmmm q y yy yyyy h hh n nn s ss ttttt` and `AM/PM am/pm A/P a/p AMPM`. `m` after `h` or before `s` is minutes.
  - String formats: `@ & < > !`.
  - Rounding is half away from zero on the decimal digits, so `Format(2.5, "0")` is "3".
- `FormatNumber`, `FormatCurrency`, `FormatPercent` with digits, leading digit, parentheses and grouping arguments. `FormatDateTime` with vbGeneralDate to vbShortTime.
- `Str` writes a leading space for non-negative numbers. `Val` reads the leading number, skips spaces, tabs and line feeds, and reads `&H`/`&O`.
- CStr, concatenation and Debug.Print write Doubles with 15 significant digits, as VBA does: `CStr(0.1 + 0.2)` is "0.3" and `CStr(0.00001)` is "1E-05". Dates print as "1/31/2024 1:45:00 PM", omitting a zero date or a midnight time.
- Locale: invariant en-US throughout. Output uses "." decimals, "," thousands, "$", m/d/yyyy dates and 12-hour times. Input reads m/d/y and ISO dates and English month and day names.

### Collection and Scripting.Dictionary

- `Collection`, created with `New Collection` or `Dim c As New Collection`:
  - `Add item, key, before, after`, also with named arguments; `Item` by 1-based index or case-insensitive key; `Count`; `Remove`; `For Each`; `c(i)` as the default member.
  - Errors: 457 for a duplicate key, 9 for a bad index, 5 for a bad key or for both Before and After.
- `Scripting.Dictionary`, from `CreateObject("Scripting.Dictionary")`, `New Dictionary` or `New Scripting.Dictionary`:
  - `Add`, `Item` get and let (`d(k) = v`, `Set d(k) = obj`), `Exists`, `Keys`, `Items`, `Remove`, `RemoveAll`, `Count`, `CompareMode`, `Key(old) = new`, `For Each` over the keys.
  - Keys are numbers, strings or objects; 1 and 1# are the same key. Reading a missing key adds it with Empty, as Scripting.Dictionary does.
  - Errors: 457 for a duplicate key, 32811 for removing a missing key.
- `CreateObject` serves "Scripting.Dictionary" itself and asks the host hook for any other ProgID.

### More built-ins

- `IIf`, `Choose`, `Switch`, `StrConv` (upper, lower and proper case), `ChrW`, `AscW`, `LenB`, `Filter`.
- `InStr`, `InStrRev`, `Replace` and `Split` take their full argument lists: start, count, limit and compare.
- `Rnd` and `Randomize` reproduce VBA's 24-bit generator. `Rnd(-1)` is 0.224007…, and `Rnd(0)` repeats the last number.
- `Hex` and `Oct` of negative numbers use 16 bits for Integers and 32 bits for Longs.
- `CVErr` and `IsError` with a new `VError` value (VarType 10). `CCur`, `CDec` (returns a Double), `CVar`, `CLngLng`, `CDate`.
- `Environ` asks the new `RuntimeHooks.Environ` hook. `DoEvents` and `Beep` do nothing.
- `Erase a, b` resets the elements of fixed-size arrays (typed defaults, fresh UDTs) and deallocates dynamic arrays. `Dim a()` is an array (`IsArray` is True, `UBound` raises 9) until `ReDim`.
- Statements: `Mid(s, start[, length]) = text`, `LSet` and `RSet`.
- `CallByName obj, name, VbMethod | VbGet | VbLet | VbSet, args...` on VB and host objects.
- `TypeName` and `VarType` cover Date, Currency and Error. `VarType` of an array is 8204, vbArray + vbVariant.
- `String(n, code)` accepts a character code.
- `Asc("")`, a `Mid`/`Left`/`Right` start below 1, a negative length, `Sqr` of a negative and `Log` of a non-positive number raise error 5.
- String functions return Null for Null.
- `IsNumeric(Empty)` is True and `IsNumeric("&HFF")` is True.
- `Debug.Print x;` (or `,`) at the end continues the line.

### Class modules

- `Event`, `RaiseEvent` and `WithEvents`:
  - `Private WithEvents src As Source` (or `Dim WithEvents`) in a standard, document or class module connects that module's `src_<Event>` procedures when `Set src = ...` runs.
  - `RaiseEvent` calls them in order and passes ByRef arguments back, so a `Cancel` argument works.
  - Events of host objects are not delivered.
- `Implements IShape`: members named `IShape_X` answer calls to `X` through any variable, unless the class has its own `X`. `TypeOf x Is IShape` is True.
- `Dim x As New Class` creates the object on first use and again after `Set x = Nothing`. This works for module, procedure and class-field variables and for Collection and Dictionary.
- `Class_Terminate` runs when an object becomes unreachable, at three points:
  - when a procedure returns holding it in a local;
  - when `Set` overwrites a variable that held it, or a Collection, Dictionary or array that may hold it;
  - when a run or a session call ends.

  Reachability is checked from module variables, active locals, the stack, object fields, arrays, Collections and Dictionaries. References kept only inside other host objects are not seen. `End` skips Class_Terminate, as in VBA.
- Default members:
  - `Attribute Value.VB_UserMemId = 0` in a class module, or VBScript's `Public Default`, makes the member the default.
  - `x = obj`, `obj + 1`, `"v=" & obj`, `Len(obj)` and comparisons read it.
  - `obj = 5` calls its Property Let, and `obj(2)` and `obj(2) = v` use it with arguments.
- Assignment through an object variable's default member: after `Set r = Range("A1")`, `r = 5` assigns the Range's value. This applies to Variant and object variables, and to VB objects with a default Property Let.
- `obj.Prop(i) = v` calls `Property Let Prop(i, v)`.
- Fixed-size array fields in classes (`Private items(10)`) are created with each instance. In 0.3.0 they were Empty.

### Statements and compile checks

- `Static x As Long` and `Static Sub/Function` keep values between calls. In class modules a static is shared by all instances.
- Numeric line labels (`10 x = 1`, `GoTo 100`, `On Error GoTo 100`, `Resume 40`). `Erl` returns the last line number reached in the failing procedure.
- `Option Explicit` is enforced at compile time. An undeclared name is a compile error, "Variable not defined: x", with its line. Immediate-window code is exempt, and ReDim may still declare.
- `Stop` returns the new `ExecutionStatus.Paused location` from `Session.Step` and `RunToEnd`; stepping again resumes. Outside a session it does nothing.
- `End` stops the program. In a session, module variables return to their initial values: the module-level code runs again and host globals are restored.
- Type names may be qualified (`As Scripting.Dictionary`, `New Scripting.Dictionary`). `As String()` return types work. In `As String * n` the length is accepted and ignored.
- `Debug.Print .Count` inside `With` reads the With object. A `.` after a space starts a With member.

### Currency and Decimal

- New `VCurrency` value: a 64-bit count of 1/10000. `Dim c As Currency`, `CCur`, and +, − and × keep exact four-decimal precision with overflow error 6: ten times 0.1 is exactly 1.
- Currency combined with Integer or Double gives Currency, following VBA's order of precision. `/` gives a Double.
- `Round`, `Abs`, `Int`, `Fix`, `Format` and `CStr` keep Currency precise.
- Decimal is still a Double: `CDec` and `As Decimal` give Doubles, and `As Decimal` was Currency in 0.3.0.
- `LongLong` and `CLngLng` keep whole numbers beyond the Long range as Doubles.

### Performance

- The dispatcher is smaller: the bodies of method calls and member stores moved to their own functions.
- Field access no longer lowercases member names. On .NET, field dictionaries ignore case, and the lowercase names that class maps need are cached.
- Concatenating two strings takes a fast path.

### Migrating from 0.3.0 (hosts)

1. Add `Environ` to `RuntimeHooks` record expressions, for example `Environ = fun _ -> ""`. Code that copies `{ defaultHooks with ... }` needs no change.
2. Handle `ExecutionStatus.Paused location` where you match on `Step`, `Run` or `RunToEnd`. A `Stop` in a macro now returns it. To ignore Stop, call `RunToEnd` again; to show a breakpoint, show the location.
3. `Value` has three new cases: `VDate`, `VCurrency` and `VError`. Matches over `Value` get incomplete-match warnings; handle the cases or add a fallback.
4. Return dates as dates. A spreadsheet host should return `VbDate.ofSerial serial` for date-formatted cells instead of the number, and accept `VDate` (and `VCurrency`) on writes: store the serial, or the value, and give the cell a date format. Reading `VbDate.tryToSerial` covers both Dates and numbers.
5. Behaviour changes that scripts may notice:
   - `x = value` where x holds an object now assigns the object's default member. Before, it replaced the variable.
   - `Option Explicit` modules with undeclared names no longer compile. Hosts must supply every constant the macros use (`xlUp`, ...) as host globals.
   - Doubles print with 15 digits.
   - `IsNumeric(Empty)` is True and `VarType(array)` is 8204.
   - `As Decimal` is a Double.
6. Optional: derive host objects that behave like dictionaries from `HostObject` and override `SetIndexed`, so that `obj(k) = v` reaches them with the key.
7. Compiler-level API: `Compiler.CompileModule` has a `DefaultMember` field, `Bytecode.ClassDef` has `DefaultMember` and `Implements`, `BytecodeProgram` has `EventBindings`, and `VBObject` has `Sinks`.

### Bytecode

Format version 4 adds:
- constant tags for Date, Currency and Error;
- opcodes for line labels and Erl, Stop and End, Let-stores through default members, indexed member assignment, Erase, events, Class_Terminate checks and CallByName;
- class default members and interfaces;
- event bindings.

Version 3 blobs are rejected.

### Benchmarks

Each figure is the minimum of 7 interleaved runs of the 0.3.0 CLI and the 0.4.0 CLI on the same scripts. The runs were pinned to two cores on a shared machine with load around 6–8. Ranges cover separate sessions, and the noise is about ±10%.

| Benchmark | 0.3.0 | 0.4.0 |
| --- | ---: | ---: |
| bench_matrix (50×50 multiply) | 0.315–0.356 s | 0.198–0.207 s |
| bench_objects (500 particles × 100 steps) | 0.323–0.340 s | 0.176–0.195 s |
| bench_recursive Fib(28) | 0.431–0.453 s | 0.362–0.408 s |
| bench_recursive Ack(3,7) | 0.209–0.229 s | 0.212–0.223 s |
| bench_recursive Hanoi(20) | 0.715–0.792 s | 0.697–0.751 s |
| bench_recursive QuickSort 5000 | 0.059 s | 0.060–0.061 s |
| bench_sieve (100000) | 0.310–0.392 s | 0.223–0.243 s |
| bench_strings | 0.038–0.040 s | 0.032–0.033 s |

The matrix, objects and sieve gains come mostly from the smaller dispatcher. bench_objects also gains from field access without lowercasing. Programs without Class_Terminate pay nothing for it: the checks are skipped at compile time, or cost one counter test per return.

### Verification

- Native: `dotnet run --project tests/Rbt.Vb.Tests.fsproj -c Release` passes 60 public-API groups, the value-semantics suite, 45 0.3.0 release groups, 42 new 0.4.0 groups, persistence and the fixtures.
- Fable 5.27.0 / Node: the same shared suites pass, including all 42 0.4.0 groups.

### Not in this release

- Decimal is a Double, `String * n` does not pad, and LongLong beyond the Long range is a Double.
- Events raised by host objects (for example `WithEvents app As Application`) are not delivered.
- `Static` variables in class modules are shared by all instances.
- `DatePart("ww")` returns 53 for the last days of some years rather than rolling over to week 1 (VBA's DatePart does the same).
- Currency literals (`5@`) are still Doubles; `CCur(5)` is a Currency.
- Locale-specific formatting other than en-US.

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
