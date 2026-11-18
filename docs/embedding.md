# Embedding Rbt.Vb

Rbt.Vb compiles VBA / VBScript to bytecode and runs it on a stack VM. The same
F# sources build for .NET and, through Fable, for JavaScript. This page
describes the host API in `Api.fs` (module `Rbt.Vb.Api`, auto-opened) and the
host-object contract in `Values.fs`.

There are two levels:

- **Single source** – `runSource`, `compileSource`, `executeWithHostGlobals`,
  `executeWith`. A script runs to the end synchronously. These are the 0.1/0.2
  entry points and keep their shape.
- **Projects and sessions** – several modules compiled together, a procedure
  list for a macro picker, named-procedure calls, an Immediate window, and
  cooperative execution in slices with cancellation.

## Values

`Values.Value` is the VB Variant. A Variant's subtype is what `TypeName` and
`VarType` report, and it decides how arithmetic overflows, so a host returns
the subtype VBA would:

| Case | VBA type | VarType | Typical host values |
| --- | --- | ---: | --- |
| `VInteger n` | Integer, −32768..32767 | 2 | Integer-typed properties (few in the Office object models) |
| `VLong n` | Long, 32-bit | 3 | `Row`, `Column`, `Count`, `Index`, Excel constants (`xlUp`), `MsgBox` results |
| `VSingle f` | Single (`float32`) | 4 | rarely; `Values.single x` rounds a Double |
| `VDouble d` | Double | 5 | cell numbers, `Interior.Color`, `ColumnWidth` |
| `VByte b` | Byte, 0..255 | 17 | rarely |
| `VString`, `VBoolean`, `VEmpty`, `VNull`, `VNothing`, `VArray`, `VObject`, `VHostObject` | | | |

A `VInteger` outside −32768..32767 is read as a Long (as in 0.4.0), but
return `VLong` for Longs: `TypeName(ws.Cells(1, 1).Row)` is "Long" in VBA.
Under Fable a `float32` is a JavaScript number; build Singles with
`Values.single` (or `Values.toSingle`), which rounds to Single precision on
both runtimes.

It also has:

- `VDate serial`, a **Date**: an OLE Automation date, the number of days
  since 1899-12-30 with the time of day as the fraction. These are Excel's
  serial numbers (1900 date system) for every date from 1900-03-01 on, so a
  host stores a date cell's serial unchanged and returns it as `VDate`.
  `TypeName` is "Date", `VarType` 7.
- `VCurrency units`, a **Currency**: a 64-bit count of 1/10000 units
  (`CCur(1.5)` is `VCurrency 15000L`). VarType 6.
- `VError number`, an **Error** value (`CVErr(2042)` is Excel's #N/A).
  `IsError` is True, VarType 10.

The `Api.VbDate` module converts dates for hosts:

```fsharp
VbDate.ofParts 2024 1 31 13 45 0      // VDate 45322.5729...
VbDate.ofDateTime DateTime.Now        // VDate (the DateTime's Kind is ignored)
VbDate.ofSerial 45322.0               // an Excel serial as a Date
VbDate.tryToSerial value              // Some serial for a Date or a number
VbDate.tryToParts value               // Some (year, month, day, hour, minute, second)
VbDate.tryToDateTime value            // Some DateTime (Kind Unspecified)
```

A spreadsheet host returns `VbDate.ofSerial serial` for cells formatted as
dates (and numbers for other cells), and accepts `VDate` when a script
writes a cell: store the serial and give the cell a date format. Scripts
then get VBA's behaviour: `Range("A1") + 7` is a Date, `Format(Range("A1"),
"yyyy-mm-dd")` and `Year(Range("A1"))` work, and `CStr` prints "1/31/2024".

Text conversions use invariant **en-US** conventions: CStr and Format write
"." decimals, "," thousands, "$" currency, `m/d/yyyy` dates and 12-hour
times with AM/PM; CDate and date literals read m/d/y (and ISO yyyy-mm-dd)
and English month and day names. Doubles print with 15 significant digits
and Singles with 7, as VBA does (`CStr(0.1 + 0.2)` is "0.3", `CStr(Rnd)` is
"0.9078093"); fixed notation is used while at most 15 (7) digits follow the
decimal point (`CStr(0.000001)` is "0.000001", `CStr(1E-16)` "1E-16").
Decimal (`CDec`, `As Decimal`) is a Double.

### Arithmetic types

Operators give their result the type of the most precise operand (Byte <
Integer < Long < Single < Double < Currency; Boolean counts as Integer, a
numeric String as Double), with VBA's exceptions (Single with Long is Double,
`/` is Double or Single, `\` and `Mod` are Byte, Integer or Long). When the
result does not fit:

- if both operands have declared types – literals, typed variables and
  functions, constants, Enum members and the built-ins VBA declares with a
  type (`Len`, `InStr`, `UBound`, `CInt`, `CLng`, `Err.Number`, …) – the
  operation raises error 6, so `30000 + 30000` and `Len(s) * 70000 * 70000`
  overflow as in VBA;
- otherwise (a Variant operand: a variable without `As`, a host property, an
  array element) the result widens, Integer to Long to Double, as VBA's
  Variant arithmetic does.

A host value therefore never overflows by itself: `ws.Cells(r, c).Row * 100000`
is a Variant expression and widens.

## Host objects

```fsharp
type IHostObject =
    abstract TypeName: string
    abstract GetMember: name: string -> Value option
    abstract SetMember: name: string -> value: Value -> bool
    abstract CallMethod: name: string -> args: Value array -> Value option
    abstract CallNamed: name: string -> args: Value array -> names: string array -> Value option
    abstract Enumerate: unit -> Value seq option
```

- `None` / `false` means *the object has no such member*. The engine raises
  error 438 ("Object doesn't support this property or method"). A member that
  exists but has no value returns `Some VEmpty`.
- `GetMember name` for a property read, `obj.Name` without arguments. When it
  returns `None`, the engine tries `CallMethod name [||]`.
- `CallMethod name args` for `obj.Name(args)` and `obj.Name args`. When it
  returns `None`, the engine tries `GetMember name` and indexes its result.
- **Assigning to a variable that holds an object** goes through the object's
  default member, as in VBA: after `Set r = Range("A1")`, `r = 5` calls
  `SetMember "" 5` on the Range (0.3.0 replaced the variable). Variables
  declared with a scalar type, and `Set`, are unaffected.
- **Default member**, the empty name `""`:
  - `obj(args)` calls `CallMethod "" args`, then `"_Default"`, then `"Item"`.
  - Reading the object as a value (Let-coercion: `x = Range("A1")`,
    `Range("A1") + 1`, `Cells(1, 1) = "x"`, `s & Cells(r, 2)`, `Len(Range("A1"))`,
    `MsgBox Range("A1")`) calls `GetMember ""`, then `"_Default"`, then
    `"Value"`. In a plain assignment an object without a default member is
    stored as the object (so `x = obj` without `Set` keeps working).
    `Set r = Range("A1")` always binds the object.
  - `obj(args) = v`, for example `Range("A1") = 5`, `Cells(r, c) = x` or
    `ws.Range("B1") = 9`, calls `CallMethod "" args` (or `ws.CallMethod "Range"`)
    and then `SetMember "" v` on the result.
- `CallNamed` is used only when a call has at least one named argument
  (`ws.Range("A1").Find What:="x", LookAt:=xlWhole`). `names.[i]` is the name
  of `args.[i]` or `""` for a positional argument. Return
  `this.CallMethod name args` to ignore names.
- `Enumerate ()` backs `For Each x In obj`. The sequence is read into an
  array when the loop starts. `None` raises error 451 ("Object not a collection").
- Omitted arguments, as in `Address(, False)` or `Worksheets.Add(, ws)`,
  arrive as `VUndefined` (`IsMissing` is True for them in VB code).
- Arguments arrive dereferenced (plain values, no `VRef`).
- **Errors**: raise `VbError { Number = 1004; Description = "..."; Source = "..." }`
  (or `raiseVbError 1004` / `raiseVbErrorWith 1004 "text"`) from any member.
  `On Error` handlers see that number, and `Err.Source` defaults to the project
  name when `Source` is empty. Any other exception becomes error 440
  ("Automation error") with the exception message as its description.

`HostObject(typeName)` is an abstract base class with defaults for every member
(no members, positional `CallNamed`, not enumerable). Override only what you
need. It also has `SetIndexed name args value` for `obj(args) = value` (name
"") and `obj.Name(args) = value`: return true when the object stores the
value itself (as a dictionary does), or false, the default, for the usual
behaviour of calling `CallMethod name args` and then `SetMember ""` on the
result:

```fsharp
type CellRange(sheet: Sheet, address: string) =
    inherit HostObject("Range")
    override _.GetMember name =
        match name with
        | "" | "Value" -> Some (sheet.Read address)
        | "Address" -> Some (VString address)
        | _ -> None
    override _.SetMember name value =
        match name with
        | "" | "Value" -> sheet.Write(address, value); true
        | _ -> false
```

### Host constants

Pass constants as host globals whose value is a plain `Value`:

```fsharp
let excelConstants = Map.ofList [ "xlUp", VLong -4162; "xlCenter", VLong -4108; ... ]
ProjectOptions.vba |> ProjectOptions.withHostGlobals (Map.fold (fun m k v -> Map.add k v m) objects excelConstants)
```

They take one global slot each, are resolved at compile time, and read like
any variable. Script code can assign to them, as it can to any VBA variable.
The built-in `vb*` constants (`vbCrLf`, `vbYesNo`, `vbObjectError`, `vbString`, …)
are compiled in. A host global or script declaration with the same name
takes precedence.

### Ending a Step slice from a host call

A host member that should give the browser a chance to run – `DoEvents`,
`Application.Wait`, a progress update – calls `Values.HostSignals.requestYield ()`.
The VM finishes the current statement's host call and returns `Running` from
`Session.Step` at the next statement boundary; the next `Step` continues.
`RunToEnd` and the single-source entry points keep running. The built-in
`DoEvents` requests a yield itself.

### Bracket evaluation

`[A1]`, `[A1:B2]` and `[Name]` compile to `Evaluate("A1")`: a host global or
procedure named `Evaluate` when there is one, else `Application.Evaluate`. A
host returns a Range object for references, so `[A1] = 10` assigns through its
default member and `[A1].Value` reads it. Without either name `[A1]` raises
error 424.

## Runtime hooks

```fsharp
type RuntimeHooks = {
    Print: string -> unit                                   // Debug.Print, Print, WScript.Echo
    MsgBox: string -> int -> string -> int                  // prompt, buttons, title -> vbOK..vbNo
    InputBox: string -> string -> string -> string option   // prompt, title, default -> text / None = Cancel
    CreateObject: string -> Value option                    // progId -> object / None = error 429
    Environ: string -> string                               // Environ("NAME") -> value / "" when unknown
}
```

`Api.defaultHooks` keeps the console behaviour: it prints, `MsgBox` returns
vbOK (1), `InputBox` reads a line on .NET and returns the default under Fable,
`CreateObject` raises 429, and `Environ` returns "". Pass hooks with `RunOptions.withHooks` or
`ProjectOptions.withHooks`, or call `session.SetHooks`. The hooks are
synchronous. A browser host that needs an asynchronous dialog can make the
hook return a placeholder and drive execution in slices.

`CreateObject("Scripting.Dictionary")` is served by the engine before the
hook is asked, and so are `New Collection`, `New Dictionary` and
`New Scripting.Dictionary`. They are `HostObject`s
(`Collections.VbCollection`, `Collections.VbDictionary`) that a host can
also create, pass in and inspect (`Count`, `Items`, `Keys`, `Item`).

`Debug.Print` follows VBA formatting: `;` joins items, numbers get a sign
position and a trailing space, and `,` inserts a tab.

## Projects

```fsharp
type ModuleKind = Standard | Class | Document
type ModuleSource = { Name: string; Kind: ModuleKind; Source: string }

Project.compile : ProjectOptions -> ModuleSource list -> Result<Project, VbFailure list>
Project.ofSource : ProjectOptions -> string -> Result<Project, VbFailure list>
Project.procedures : Project -> ProcedureInfo list
Project.macros : Project -> ProcedureInfo list          // public Subs without required parameters
Project.tryFindProcedure : string -> Project -> ProcedureInfo option
ModuleSource.parseHeader : string -> ModuleHeader         // VB_Name, attributes, body
```

- `VERSION … CLASS`, `BEGIN … END` and `Attribute …` lines are blanked rather
  than removed, so line numbers still match the text you passed in.
  `parseHeader` returns the `VB_Name` and the other attributes.
- Procedures are registered as `Module.Proc`. Public procedures of
  *standard* modules are also registered under their bare name, and the first
  module wins on a clash. Inside a module, its own procedures and Private
  variables shadow the public ones. `Module2.Helper` and `Module1.Counter`
  qualify explicitly.
- Public module-level variables are shared by all modules. Private ones are
  per module and readable from the host as `"Module.Name"`.
- A *class* module is compiled as a class named after the module, with
  fields, methods, `Property Get/Let/Set` (with arguments), `Class_Initialize`,
  `Class_Terminate`, `Event`/`RaiseEvent`, `Implements` and a default member
  (`Attribute Value.VB_UserMemId = 0` inside the member, as exported .cls
  files have it; VBScript's `Public Default` also works).
- `Private WithEvents src As Source` in a standard, document or class module
  connects that module's `src_<Event>` procedures when `Set src = ...` runs;
  `RaiseEvent Changed(...)` in Source calls them, ByRef arguments included.
  Events of host objects are not delivered.
- `Implements IShape`: members named `IShape_Area` answer calls to `Area`
  (a class's own member of the same name wins), and `TypeOf x Is IShape` is
  True.
- **Class_Terminate** runs once an instance is unreachable: after a
  procedure returns holding it in a local, after `Set` overwrites a variable
  that held it, and when a run or a session call ends. Reachability is
  checked from module variables, the active procedures' locals, the stack,
  object fields, arrays, Collections and Dictionaries; references kept only
  inside other host objects do not count. `End` does not run it.
- `Static` locals and `Static Sub/Function` keep their values between calls
  (one copy per procedure; in class modules it is shared by all instances).
- `Option Explicit` is enforced at compile time ("Variable not defined: x",
  with its line). Immediate-window code is exempt.
- A *document* module (ThisWorkbook, Sheet1) compiles like a standard module,
  but its procedures are reachable only by qualified name.

```fsharp
type ProcedureInfo = {
    Name: string; Module: string; ModuleKind: ModuleKind; QualifiedName: string
    Kind: ProcedureKind              // SubProcedure | FunctionProcedure | PropertyGet/Let/SetProcedure
    Parameters: ParameterInfo list   // Name, Optional, ByRef, IsParamArray, TypeName, DefaultValue
    IsPublic: bool
    Line: int; EndLine: int          // 1-based declaration and End lines
}
    member IsMacro: bool
```

## Sessions

A `Session` is one running instance of a project. Module-level state survives
between calls. Only one execution runs at a time.

```fsharp
Session.Start : Project -> Session                 // module-level code is queued
Session.Create : Project * ?maxInstructions -> Result<Session, VbFailure>  // Start + RunToEnd

// Begin an execution (Error if one is already running):
session.Call(name: string, args: Value list) : Result<unit, VbFailure>     // "Module.Proc" or "Proc"
session.Evaluate(expression, ?moduleName) : Result<unit, VbFailure>
session.Execute(statements, ?moduleName) : Result<unit, VbFailure>
session.Immediate(line, ?moduleName) : Result<unit, VbFailure>             // "?expr", "Print expr" or statements

// Drive it:
session.Step(maxInstructions: int) : ExecutionStatus     // Running | Finished of Value | Failed of VbFailure | Paused of Location
session.RunToEnd(?maxInstructions) : ExecutionStatus
session.Cancel()                                          // next Step fails with error 18
session.Location : Location option                        // { Line; Procedure; Module } while paused
session.IsRunning, session.Status

// Shortcuts: Call/Immediate followed by RunToEnd
session.Run(name, args, ?maxInstructions) : ExecutionStatus
session.RunImmediate(line, ?moduleName, ?maxInstructions) : ExecutionStatus

session.TryGetGlobal(name) / session.SetGlobal(name, value) / session.SetHooks(hooks)
```

- `Finished value` carries a Function's return value or the value of an
  evaluated expression, and `Empty` otherwise. `displayValue` formats a value
  the way the Immediate window shows it.
- Immediate-window variables that are not declared anywhere (`x = 5`) become
  session variables, so a later `?x` prints 5. Pass `moduleName` to evaluate
  in a module's scope, where its Private variables and procedures are visible.
- Omitted Optional arguments of `Call` take their literal default, the type
  default, or Missing.
- After `Failed`, `Finished`, or a cancellation the session is idle again and
  keeps its module state.
- **Stop** makes `Step` and `RunToEnd` return `Paused location` (the line of
  the Stop statement); `Step`/`RunToEnd` again continues after it. Outside a
  session (`runSource`, `execute`) Stop does nothing.
- **End** finishes the execution with `Finished Empty` and resets module
  state as VBA does: module variables return to their initial values (the
  module-level code runs again) and host globals are restored. Class_Terminate
  does not run for the discarded objects.

### Cooperative execution in a browser

```fsharp
let rec pump () =
    match session.Step 20000 with
    | Running -> showLine session.Location; window.setTimeout(pump, 0) |> ignore
    | Finished value -> done value
    | Failed failure -> showError (VbFailure.format failure)
    | Paused location -> showBreakpoint location   // Stop; resume with pump ()
session.Call("Module1.Main", []) |> ignore
pump ()
// Stop button:
stopButton.onclick <- fun _ -> session.Cancel()
```

## Errors

```fsharp
type VbErrorKind = Syntax | Compile | Runtime
type VbFailure = {
    Kind: VbErrorKind
    Number: int              // VB error number for run-time errors (0 for syntax/compile)
    Description: string
    Source: string           // Err.Source (the project name for engine errors)
    Line: int                // 1-based line in the module source
    Column: int              // syntax errors
    Procedure: string        // without the module prefix; "" at module level
    Module: string
}
VbFailure.format : VbFailure -> string   // "Run-time error '11': Division by zero (Module1.Boom, line 21)"
```

The single-source API raises `StackVm.VbRuntimeException`, which carries
`.Failure` (`StackVm.RuntimeFailure` with the same fields), from
`execute`/`executeWithHostGlobals`. `runSource` returns
`RunError.RuntimeError message` with the formatted text.

Run-time error numbers include 3, 5, 6, 7, 9, 11, 13, 18, 20, 28, 35, 91, 94,
424, 429, 438, 440, 448, 449, 450, 451, 457, 32811, and any number passed to
`Err.Raise`. `Erl` is the last numeric line label (`30 x = 1 / 0`) reached in
the procedure that failed, or 0.
