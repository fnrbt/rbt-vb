# Changelog

## 0.8.3 — 2026-11-18

- A variable declared As a class starts as Nothing, as in VBA: the project's own classes, VBA's (Collection, Dictionary ...) and, with `ProjectOptions.withHostTypes`, the host's (Range, Shape, Document ...), for locals, module variables, class fields and array elements. It started as Empty, so `If s Is Nothing Then` after a failed `Set s = Shapes("x")` under On Error Resume Next was False. A variable As an enum (the project's, or the host's: XlDirection) starts as a Long 0. Unknown type names still start as Empty. (`Compiler.compileModulesWith` takes the host's object and enum type names.)
- `Debug.Print (a); (b)` parses: a parenthesised first item followed by `;`.

## 0.8.2 — 2026-11-18

- Compile errors whose VBA text names nothing ("Variable not defined", "Named argument not found", "Label not defined") have VBA's text as their Description, and the name in Detail; it was appended to the text ("Variable not defined: y"). Matches Excel 2019 (rbt-office's compile parity cases: 22 of 22).
- The host API's source file is `Api.fs` (it was `FsVb.fs`, from the project's first name); its module is `Rbt.Vb.Api` as before.

## 0.8.1 — 2026-11-18

- Early binding follows default members: arguments given to a member without parameters go to its result's default member (`ActiveDocument.Paragraphs(1)` is a Paragraph, `ps(1)` for `ps As Paragraphs` too), as the library's DISPID 0 says (a `default` sixth field of a member line). 0.8.0 took `Paragraphs(1)` for the collection and reported its members as missing.
- `VbFailure.format` shows a syntax or compile error's `Detail` as it does a run-time error's: `Compile error: Method or data member not found [Range.Frobnicate] (Module1.Main, line 2)`.

## 0.8.0 — 2026-11-18

### Early binding (TypeCheck)

A host can give its project the types of its object model (`ProjectOptions.withHostTypes`), as TSV made from its type libraries (`TypeCheck.Library.parse`; rbt-office's `tools/vba-parity/typelib.ps1` writes Excel's and Word's). Members of declared types (`Dim r As Range`, parameters, function results, `With`, typed globals such as `Range(...)`, document modules' `Me`, VBA classes) are then checked when the project compiles, as VBA's compiler checks them. The rules come from Microsoft Excel 2019 compiling the same cases:

- A member a **non-extensible** interface does not have (Excel's Worksheet, Word's Range and most of Word's, a VBA class's private or missing member) is "Method or data member not found". An **extensible** interface (Excel's Range, Font, Application) may have members its library does not list: they are found when they run (error 438 then).
- A known member called with more arguments than it has parameters is "Wrong number of arguments or invalid property assignment"; with a name it has no parameter of, "Named argument not found". A member without parameters that returns an object passes its arguments to that object's default member (`r.Cells(1, 1)`).
- A property without Let written is "Wrong number of arguments or invalid property assignment", unless it returns an object (the value goes to its default member, `ws.UsedRange = 1`); one without Set given `Set` is "Invalid use of property".
- Object, Variant and types no library describes are bound late, as before.

The errors are compile failures (`VbFailure`, Kind Compile) with the member in `Detail`. `TypeCheck.Library` also carries the libraries' enum constants. `ProjectOptions` has a new field (`HostTypes`), so this is 0.8.0.

## 0.7.0 — 2026-11-18

Late-bound calls on host objects raise what VB raises, as Microsoft Excel 2019 answers them (a case of rbt-office's VBA parity run). `VbErrorInfo`, `RuntimeFailure` and `VbFailure` have a new field, so this is 0.7.0.

- **Err.Description is VB's text.** Error 438 is "Object doesn't support this property or method" and 448 "Named argument not found", with nothing added. What the error was about (the type and member, "Range.Nope"; the argument's name) is in the new `Detail` field of `VbErrorInfo`, `RuntimeFailure` and `VbFailure`, and `VbFailure.format` shows it: `Run-time error '438': Object doesn't support this property or method [Range.Nope] (Module1.Main, line 3)`. `raiseVbErrorAbout number detail` raises an error with VB's description and a detail.
- **Read-only properties** (`HostMembers` members without a setter): written, error 450 "Wrong number of arguments or invalid property assignment"; `Set` of an object, error 13. A value written to a read-only property that reads as an object goes to that object's default member (`ws.UsedRange = 1` fills the used range, as Excel's).
- **Argument counts**: a method given more arguments than its table names parameters is error 450; a property that is not a collection given arguments is 450 (it was 13).
- **CallByName ... VbMethod** calls methods only: a property of a `HostDispatch` object called so is error 438. `HostDispatch.IsMethod` says which members are methods (true unless a host says otherwise).

## 0.6.1 — 2026-11-18

### Members of host objects by number

- `HostDispatch`, an abstract class a host object can inherit: its members have numbers shared by every object of its type (`DispatchType`, `Resolve`, `GetById`, `SetById`, `CallById`). The engine finds a member's number by name the first time an instruction meets an object of that type, then reaches it by number: a loop over `Cells(r, c).Interior.Color` looks no name up after its first pass. An instruction that meets objects of another type finds the number again.
- `HostMembers.Table` objects are `HostDispatch` objects (a member's number is its place in the table), so object models built from tables get this without a change.
- Host objects that implement only `IHostObject` are asked by name, as before.

## 0.6.0 — 2026-11-18

### HostMembers: an object model as tables

`Rbt.Vb.HostMembers` builds a host's object model from tables. Each type of object (a Range, a Shape, a Document) is a `Table<'T>` of members, built once; an object is the table with one instance's state (`table.Wrap state`). A member is found by name in any case with one dictionary lookup, so an object model no longer has to compare a name against each member it knows on every access.

- Members: `get` (read only), `prop` (read and write), `set` (write only), `method` (with its parameters' names), `indexed` (a property that takes arguments, as `Cells(r, c)`), `accepted` (a setting with no effect that reads as a fixed value), `alias`.
- Named arguments are put in their places from the method's parameter names; an unknown name is error 448.
- A name the table does not have is error 438, naming the type and the member: "Object doesn't support this property or method (Shape.Nope)".
- `defaultMember` serves `obj(...)`, `obj = x` and the object read as a value; `items` makes the object a collection for For Each; `dynamic` handles names that are not in the table (a WorksheetFunction object that takes any function's name); `typeNameOf` gives a TypeName that depends on the instance.
- `table.TryState value` gives the state of an object of that table (None for any other value), and `table.Names` lists the members, for an editor's completion or documentation.

## 0.5.1 — 2026-11-18

Two gaps found by macros from the web:

- `Rem` starts a comment, as `'` does, at the start of a line or after a `:`.
- A class module's `Const` belongs to the module (Private, as VBA makes it), and the class's methods see it. Before, it was taken for an instance field and the method saw an undeclared name ("Variable not defined" under Option Explicit). Two class modules can each have a constant of the same name.

## 0.5.0 — 2026-11-18

This release makes values carry VBA's numeric subtypes. The same macros were run in Microsoft Excel 2019 and in Rbt.Vb, and the differences in types, overflow, number formatting, Like, Erl and parsing are fixed. The comparison runs as a test (832 probes in 18 modules). It changes the `Value` type, so it is 0.5.0 rather than 0.4.1. See [docs/embedding.md](docs/embedding.md).

### Integer, Long, Single and Byte

- New `Value` cases: `VLong of int` (VarType 3), `VSingle of float32` (4) and `VByte of byte` (17). `VInteger` is now the 16-bit Integer. A `VInteger` outside its range still reads as a Long.
- Literals have VBA's types: `5` and `5%` are Integers, `70000`, `5&` and `&HFFFF&` are Longs, `-32768` is a Long (the negation of the Long 32768), `1.5!` is a Single and `5@` a Currency (it was a Double).
- Declared types hold their subtype: `Dim l As Long` is a `VLong`, `As Single` a `VSingle`, `As Byte` a `VByte`. Typed parameters convert values passed to them, including Optional defaults and arguments from `Session.Call`.
- Built-ins return VBA's types:
  - Long: `Len`, `LenB`, `InStr`, `InStrRev`, `UBound`, `LBound`, `CLng`, `VarType`, `RGB`, `QBColor`, `DateDiff` (a Double beyond the Long range), `Err.Number`, `Erl`, `MsgBox`, `Collection.Count`, `Dictionary.Count`, Enum members and the `vb*` constants.
  - Integer: `CInt`, `Asc`, `AscW`, `Sgn`, `StrComp`, `Year`, `Month`, `Day`, `Weekday`, `Hour`, `Minute`, `Second`, `DatePart`.
  - Single: `CSng`, `Rnd`, `Timer`. Byte: `CByte`, `AscB`.
  - `Int`, `Fix`, `Abs` and `Round` keep the operand's type, so `Int(2.5!)` is a Single and `Abs(True)` an Integer.
- Operators follow VBA's result types:
  - `+ - *` take the most precise operand's type (Byte < Integer < Long < Single < Double < Currency; Boolean counts as Integer, a numeric String as Double, Empty takes the other operand's type). Single with Long is a Double, and Currency times a Single, Double or String is a Double.
  - `/` is a Double, or a Single when a Single meets a Single, Integer, Byte or Boolean. `0 / 0` is error 6.
  - `\` and `Mod` give a Byte for two Bytes, an Integer for Integers and Booleans, otherwise a Long.
  - `Not`, `And`, `Or`, `Xor`, `Eqv` and `Imp` are bitwise unless both operands are Booleans: `3 Eqv 5` is -7, `Not CByte(1)` is 254. Null follows VBA's rules (`False Imp Null` is True).
- Overflow:
  - When both operands have declared types the operation raises error 6, as VBA does. These are literals, typed variables and functions, constants, Enum members, and the built-ins VBA declares with a type (`Len`, `InStr`, `UBound`, `CInt`, `CLng`, `Err.Number`, …). So `30000 + 30000`, `123456789 * 1000`, `Len(s) * 70000 * 70000`, `-i` for an Integer -32768 and a typed `For i = 32760 To 32767` loop overflow.
  - With a Variant operand the result widens: Byte to Integer to Long to Double, Single to Double. `v + w` with two Variant Integers of 30000 is the Long 60000. 0.3.0 widened to Double in every case.
  - Double overflow (`1E+308 * 10`) is error 6.
- A Variant `For` counter takes the widest type of its start, end and step. `For k = 1 To n` with n a Long counts in Longs.
- `Hex` and `Oct` use 16 bits for Integers, Bytes and Booleans, 32 for Longs and 64 for other types: `Hex(-1#)` is "FFFFFFFFFFFFFFFF".
- A Single compares with a Double at Single precision: `CSng(0.1) = 0.1` is True.

### Number text

- Singles print with 7 significant digits: `CStr(Rnd)` is "0.9078093" and `CStr(CSng(123456789))` is "1.234568E+08".
- Doubles and Singles are written in fixed notation while at most 15 (Singles: 7) digits follow the decimal point and the integer part has fewer than 15 (7) digits. So `CStr(0.000001)` is "0.000001", `CStr(1E-10)` is "0.0000000001", `CStr(1.5E-15)` is "1.5E-15" and `CStr(1 / 3 / 1000)` is "3.33333333333333E-04". 0.4.0 switched to scientific notation below 1E-4.
- `Str` writes no 0 before the decimal point (`Str(0.5)` is " .5") and no space before a Date or Boolean.
- `Format` of a Single rounds its 7 printed digits, so `Format(CSng(2.675), "0.00")` is "2.68".
- `CCur` of a large Double or of decimal text keeps every digit (`CStr(CCur(922337203685477#))` is "922337203685477").
- `IsNumeric` and `CDbl` read "$5", "5 $", "1,234", "(5)", "5-", "+5", "1D2", tabs, line breaks and non-breaking spaces as VBA does. ",5", "5%" and "1d" are not numbers.
- `Val("&777")` is octal 511, and `Val("1e400")` is error 6.

### Like

- Bracket classes support ranges (`[a-c]`, `[1-3]`), `[!a-c]`, a literal `-` at the start or end, `[[]`, and `[]` (which matches nothing). `"b7" Like "[a-c]#"` is True.
- Like is case-sensitive unless `Option Compare Text` is set, as in VBA. 0.4.0 ignored case.
- A bracket without its closing `]` is error 93.

### Erl and Resume

- `Erl` is the line number reached in the procedure that handles the error. When the error comes from a called procedure, it is the line of the call. 0.4.0 used the failing procedure's line.
- `Err.Clear` keeps `Erl`. `Resume`, `On Error` and leaving the handler reset it.
- When a procedure's first numbered lines are declarations (`10 Dim a`), Excel reports the first numbered line for an error in the first executable numbered line, unless that line is the procedure's last numbered line. Rbt.Vb reports the same.
- `Resume` in a single-line `If … Then Err.Raise 5` repeats `Err.Raise`, not the If. This was already so in the engine; a test now covers it.

### Parsing and compiling

- A module-level declaration after the first procedure is a compile error: "Only comments may appear after End Sub, End Function, or End Property". This applies to `Project.compile`; single scripts still allow it.
- `Next c` after `For Each c`.
- A local variable or parameter named like a procedure hides that procedure. For example, `Sub color()` with `Dim color` uses the variable.
- `Debug.Print ("a") & ("b")`, `Debug.Print (1 = 1) & "!"` and `Foo (1), 2`: parentheses followed by more of the statement belong to the first argument.
- An `Else` on the line after a single-line `If … Then x = 1` belongs to the enclosing block If.
- `Const A = 15, B = 4` and `Private Const X As Long = 1, Y As Long = 2`.
- `Set a(i) = obj` stores the object itself, also for host objects with a default member.
- `Show p()` passes the array `p`.
- The VBA library qualifiers resolve to built-ins: `VBA.Len`, `VBA.Left$`, `VBA.Strings.Mid`, `Strings.UCase`, `Math.Sqr`, `Interaction.MsgBox`, `DateTime.Now`, `Conversion.CLng`, `VBA.vbCrLf`.
- `[A1]`, `[A1:B2]` and `[Name]` are `Evaluate("…")`. They compile to a host global or procedure named Evaluate, else to `Application.Evaluate`. `[A1] = 10` assigns through the result's default member.

### Hosts

- `Values.HostSignals.requestYield ()` ends the current `Session.Step` slice at the next statement boundary. Hosts call it from members like `DoEvents` or `Application.Wait`; the built-in `DoEvents` calls it too.
- `Values.single` and `Values.toSingle` build Singles that are rounded the same way on .NET and under Fable.

### Migrating from 0.4.0 (hosts)

1. Return Longs as `VLong`: `Row`, `Column`, `Count`, `Index`, `ColorIndex`, `HorizontalAlignment` and other enumerations, `Err`-style numbers, and constants such as `xlUp`. A `VInteger` outside -32768..32767 still behaves as a Long, but `TypeName` of a small `VInteger` is "Integer".
2. Return VBA's Double properties (`Interior.Color`, `Font.Color`, `Font.Size`, `ColumnWidth`, `RowHeight`) as `VDouble`.
3. Matches over `Value` get incomplete-match warnings for `VLong`, `VSingle` and `VByte`. This matters most where values written by scripts are read, for example a cell store. With `| VInteger i -> ... | VDouble d -> ...`, a Long or Single written to a cell falls through to the fallback branch. Handle the new cases, or convert with `ValueSemantics.toNumber`, `toInteger` and `toString`.
4. `Ast.Literal` has the new cases `Long`, `Single` and `Currency`. `Ast.Statement.IndexedAssignment` has a fourth field, `isSet`, and there is a new `StatementGroup` case. The parser flattens statement groups, so a parsed `Program` never contains one. `TokenKind` has `BracketText`.
5. Behaviour changes that scripts may notice:
   - Integer arithmetic on declared types overflows (error 6) where 0.4.0 widened.
   - `CStr` of small Doubles is fixed-point.
   - Like is case-sensitive.
   - `TypeName` reports Long, Single and Byte.
   - `Rnd` and `Timer` are Singles.
   - Declarations after the first procedure no longer compile.

### Bytecode

Format version 5 adds:
- constant tags for Long, Single and Byte;
- the opcodes `AddChecked`, `SubtractChecked`, `MultiplyChecked`, `DivideChecked`, `IntDivideChecked`, `ModulusChecked` and `NegateChecked` for arithmetic on typed operands;
- the opcode `CoerceParams` for typed parameters.

Version 4 blobs are rejected.

### Benchmarks

Each figure is the minimum of 9 interleaved runs of the 0.4.0 CLI and the 0.5.0 CLI on the same scripts. The runs were pinned to two cores on a shared machine with load 6–8, and the noise is about ±10%.

| Benchmark | 0.4.0 | 0.5.0 |
| --- | ---: | ---: |
| bench_matrix (50×50 multiply) | 0.204 s | 0.218 s |
| bench_objects (500 particles × 100 steps) | 0.190 s | 0.189 s |
| bench_recursive Fib(28) | 0.381 s | 0.412 s |
| bench_recursive Ack(3,7) | 0.218 s | 0.222 s |
| bench_recursive Hanoi(20) | 0.699 s | 0.709 s |
| bench_recursive QuickSort 5000 | 0.061 s | 0.063 s |
| bench_sieve (100000) | 0.218 s | 0.225 s |
| bench_strings | 0.031 s | 0.028 s |

Typed arithmetic chooses its opcode at compile time, so the VM does no extra work to tell typed operands from Variant ones. Long arithmetic and the `\`/`Mod` fast paths are on par with 0.4.0's Integer paths. Parameter conversion happens inside `Call` rather than as a dispatched instruction. Fib is up to 8% slower: that is the cost of converting the Long parameter on each call.

### Verification

- Native: `dotnet run --project tests/Rbt.Vb.Tests.fsproj -c Release` passes the public-API, value-semantics, 0.3.0, 0.4.0 and 0.5.0 suites (17 new 0.5.0 groups, among them the 832 Excel probes), persistence and the fixtures.
- Fable 5.27.0 / Node: the same shared suites pass, including the Excel probes.

### Not in this release

- `Len` of a typed variable is the length of its text, not its size in bytes (`Len(s)` with `s As Single` is 4 in VBA).
- Decimal is still a Double (`TypeName(CDec(1))` is "Double"), and LongLong is a Long or a Double.
- Arrays do not keep their element type when passed through a Variant parameter: `arr(1) = 99` in `Sub F(arr)` does not convert to the caller's `As Long`.
- `Abs` of a Variant Integer -32768 raises error 6. Excel raises it for typed operands; a Variant may widen instead.

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
