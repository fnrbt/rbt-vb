# Embedding Rbt.Vb

Rbt.Vb compiles VBA / VBScript to bytecode and runs it on a stack VM. The same
F# sources build for .NET and, through Fable, for JavaScript. This page
describes the host API in `FsVb.fs` (module `Rbt.Vb.Api`, auto-opened) and the
host-object contract in `Values.fs`.

There are two levels:

- **Single source** – `runSource`, `compileSource`, `executeWithHostGlobals`,
  `executeWith`. A script runs to the end synchronously. These are the 0.1/0.2
  entry points and keep their shape.
- **Projects and sessions** – several modules compiled together, a procedure
  list for a macro picker, named-procedure calls, an Immediate window, and
  cooperative execution in slices with cancellation.

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
need:

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
let excelConstants = Map.ofList [ "xlUp", VInteger -4162; "xlCenter", VInteger -4108; ... ]
ProjectOptions.vba |> ProjectOptions.withHostGlobals (Map.fold (fun m k v -> Map.add k v m) objects excelConstants)
```

They take one global slot each, are resolved at compile time, and read like
any variable. Script code can assign to them, as it can to any VBA variable.
The built-in `vb*` constants (`vbCrLf`, `vbYesNo`, `vbObjectError`, `vbString`, …)
are compiled in. A host global or script declaration with the same name
takes precedence.

## Runtime hooks

```fsharp
type RuntimeHooks = {
    Print: string -> unit                                   // Debug.Print, Print, WScript.Echo
    MsgBox: string -> int -> string -> int                  // prompt, buttons, title -> vbOK..vbNo
    InputBox: string -> string -> string -> string option   // prompt, title, default -> text / None = Cancel
    CreateObject: string -> Value option                    // progId -> object / None = error 429
}
```

`Api.defaultHooks` keeps the console behaviour: it prints, `MsgBox` returns
vbOK (1), `InputBox` reads a line on .NET and returns the default under Fable,
and `CreateObject` raises 429. Pass hooks with `RunOptions.withHooks` or
`ProjectOptions.withHooks`, or call `session.SetHooks`. The hooks are
synchronous. A browser host that needs an asynchronous dialog can make the
hook return a placeholder and drive execution in slices.

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
  fields, methods, `Property Get/Let/Set` and `Class_Initialize`. Events and
  `Implements` are not supported yet.
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
session.Step(maxInstructions: int) : ExecutionStatus     // Running | Finished of Value | Failed of VbFailure
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

### Cooperative execution in a browser

```fsharp
let rec pump () =
    match session.Step 20000 with
    | Running -> showLine session.Location; window.setTimeout(pump, 0) |> ignore
    | Finished value -> done value
    | Failed failure -> showError (VbFailure.format failure)
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
424, 429, 438, 440, 448, 449, 450, 451, and any number passed to `Err.Raise`.
`Erl` returns 0, because numeric line labels are not supported.
