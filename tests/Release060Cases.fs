module Rbt.Vb.Tests.Release060Cases

// 0.6.0-0.7.0: HostMembers - an object model as tables of members, members by number, VB's errors for
// late-bound calls. Shared by .NET and Fable.

open Rbt.Vb
open Rbt.Vb.Api
open Rbt.Vb.Values
open Rbt.Vb.HostMembers

let private fail label expected actual =
    failwithf "%s: expected %A, received %A" label expected actual

type private Box = { mutable Left: float; mutable Text: string; Items: ResizeArray<string> }

let rec private boxType : Table<Box> =
    Table<Box>("Box",
        [ prop "Left" (fun b -> VDouble b.Left) (fun b v -> b.Left <- ValueSemantics.toNumber v)
          prop "Text" (fun b -> VString b.Text) (fun b v -> b.Text <- ValueSemantics.toString v)
          alias "Caption" (prop "Text" (fun b -> VString b.Text) (fun b v -> b.Text <- ValueSemantics.toString v))
          get "Count" (fun b -> VLong b.Items.Count)
          indexed "Item" [ "Index" ] (fun _ -> VEmpty) (fun b args -> VString b.Items.[ValueSemantics.toInteger args.[0] - 1])
          method "Move" [ "Dx"; "Times" ] (fun b args ->
              let times = match arg args 1 with VUndefined -> 1.0 | v -> ValueSemantics.toNumber v
              b.Left <- b.Left + ValueSemantics.toNumber args.[0] * times
              VEmpty)
          method "Clear" [] (fun b _ -> b.Items.Clear(); VEmpty)
          get "Self" (fun b -> boxType.Wrap b)
          accepted "Locked" (VBoolean false) ],
        defaultMember = "Text",
        items = (fun b -> b.Items |> Seq.map VString))

type private Other = { Name: string }
let private otherType : Table<Other> =
    Table<Other>("Other", [ get "Pad1" (fun _ -> VEmpty); get "Pad2" (fun _ -> VEmpty); get "Text" (fun o -> VString o.Name) ])

/// A host with numbered members that counts how often a name is looked up.
type private Counting() =
    inherit HostDispatch()
    member val Resolved = 0 with get, set
    member val X = 0 with get, set
    override _.DispatchType = box "Counting"
    override this.Resolve name = this.Resolved <- this.Resolved + 1; (if name.ToLowerInvariant() = "x" then 0 else -1)
    override this.GetById _ = Some (VLong this.X)
    override this.SetById _ value = this.X <- ValueSemantics.toInteger value; true
    override _.CallById _ _ = None
    interface IHostObject with
        member _.TypeName = "Counting"
        member this.GetMember name = if name.ToLowerInvariant() = "x" then Some (VLong this.X) else None
        member this.SetMember name value = this.X <- ValueSemantics.toInteger value; true
        member _.CallMethod _ _ = None
        member _.CallNamed _ _ _ = None
        member _.Enumerate () = None

let private runWith (globals: (string * Value) list) (code: string) =
    let output = ResizeArray<string>()
    let options =
        ProjectOptions.vba
        |> ProjectOptions.withHooks { defaultHooks with Print = output.Add }
        |> ProjectOptions.withHostGlobals (Map.ofList globals)
    match Project.compile options [ ModuleSource.standard "M" code ] with
    | Error e -> failwithf "%A" e
    | Ok p ->
        match Session.Create p with
        | Error f -> failwithf "%A" f
        | Ok s ->
            match s.Run("Main", [], 1000000) with
            | Finished _ -> List.ofSeq output
            | other -> failwithf "%A (output %A)" other (List.ofSeq output)

let private printed (b: Box) (code: string) =
    let output = ResizeArray<string>()
    let options =
        ProjectOptions.vba
        |> ProjectOptions.withHooks { defaultHooks with Print = output.Add }
        |> ProjectOptions.withHostGlobals (Map.ofList [ "B", boxType.Wrap b ])
    match Project.compile options [ ModuleSource.standard "M" code ] with
    | Error e -> failwithf "%A" e
    | Ok p ->
        match Session.Create p with
        | Error f -> failwithf "%A" f
        | Ok s ->
            match s.Run("Main", [], 100000) with
            | Finished _ -> List.ofSeq output
            | other -> failwithf "%A (output %A)" other (List.ofSeq output)

let private groups : (string * (unit -> unit)) list = [
    "members in any case, properties, methods, named arguments", (fun () ->
        let b = { Left = 1.0; Text = "a"; Items = ResizeArray [ "x"; "y" ] }
        let out = printed b "Sub Main()\n    b.LEFT = 5\n    b.move 2\n    B.Move Times:=3, Dx:=1\n    Debug.Print b.Left; b.Count; b.Item(2); b(1)\n    b.Caption = \"hi\"\n    Debug.Print b; b.Self.Text\n    b.Locked = True\n    Dim s\n    For Each s In b: Debug.Print s;: Next\n    Debug.Print\n    b.Clear\n    Debug.Print b.Count\nEnd Sub\n"
        if out <> [ " 10  2 yx"; "hihi"; "xy"; ""; " 0 " ] && out <> [ " 10  2 yx"; "hihi"; "xy"; " 0 " ] then fail "box" "..." out)

    "an unknown member: error 438 with VB's text in Err, the member in the failure's Detail", (fun () ->
        let b = { Left = 1.0; Text = "a"; Items = ResizeArray() }
        let out = printed b "Sub Main()\n    On Error Resume Next\n    b.Nope = 1\n    Debug.Print Err.Number; Err.Description\n    Err.Clear\n    x = b.Nope2\n    Debug.Print Err.Description\nEnd Sub\n"
        let expected = [ " 438 Object doesn't support this property or method"; "Object doesn't support this property or method" ]
        if out <> expected then fail "438" expected out
        let options = ProjectOptions.vba |> ProjectOptions.withHostGlobals (Map.ofList [ "B", boxType.Wrap b ])
        match Project.compile options [ ModuleSource.standard "M" "Sub Main()\n    x = b.Nope\nEnd Sub\n" ] with
        | Error e -> failwithf "%A" e
        | Ok p ->
            match Session.Create p with
            | Error f -> failwithf "%A" f
            | Ok s ->
                match s.Run("Main", [], 1000) with
                | Failed f when f.Number = 438 && f.Detail = "Box.Nope" && f.Description = "Object doesn't support this property or method" ->
                    let text = VbFailure.format f
                    if not (text.Contains "[Box.Nope]") then fail "format" "[Box.Nope]" text
                | other -> fail "unhandled 438" "Failed with Detail Box.Nope" other)

    // As Excel 2019 answers late-bound calls (tests/vba-parity/cases/dispatch.bas in rbt-office).
    "read-only properties, argument counts and CallByName as VB's", (fun () ->
        let b = { Left = 1.0; Text = "a"; Items = ResizeArray [ "x" ] }
        let code =
            [ "Sub Main()"
              "    On Error Resume Next"
              "    Dim n"
              "    b.Count = 5: n = Err.Number: Err.Clear: Debug.Print \"read-only\"; n"
              "    Set b.Count = b: n = Err.Number: Err.Clear: Debug.Print \"set read-only\"; n"
              "    b.Self = \"via default\": n = Err.Number: Err.Clear: Debug.Print \"read-only object\"; n; b.Text"
              "    b.Move 1, 2, 3: n = Err.Number: Err.Clear: Debug.Print \"too many\"; n"
              "    x = b.Left(1): n = Err.Number: Err.Clear: Debug.Print \"property with arguments\"; n"
              "    x = CallByName(b, \"Left\", VbMethod): n = Err.Number: Err.Clear: Debug.Print \"property as method\"; n"
              "    x = CallByName(b, \"Left\", VbGet): n = Err.Number: Err.Clear: Debug.Print \"property got\"; n"
              "    CallByName b, \"Clear\", VbMethod: n = Err.Number: Err.Clear: Debug.Print \"method\"; n; b.Count"
              "    CallByName b, \"Count\", VbLet, 3: n = Err.Number: Err.Clear: Debug.Print \"let read-only\"; n"
              "End Sub" ] |> String.concat "\n"
        let out = printed b code
        let expected =
            [ "read-only 450 "; "set read-only 13 "; "read-only object 0 via default"; "too many 450 "
              "property with arguments 450 "; "property as method 438 "; "property got 0 "; "method 0  0 "; "let read-only 450 " ]
        if out <> expected then fail "dispatch rules" expected out)

    "one place in the code with objects of two types: each finds its own member", (fun () ->
        let b = { Left = 1.0; Text = "box"; Items = ResizeArray() }
        let out = runWith [ "BX", boxType.Wrap b; "OT", otherType.Wrap { Name = "other" } ] "Sub Main()\n    Dim i, o, s\n    For i = 1 To 4\n        If i Mod 2 = 0 Then Set o = BX Else Set o = OT\n        s = s & o.Text & \",\"\n    Next\n    Debug.Print s\nEnd Sub\n"
        if out <> [ "other,box,other,box," ] then fail "alternating types" [ "other,box,other,box," ] out)

    "a loop looks a member's name up once", (fun () ->
        let c = Counting()
        let out = runWith [ "C", VHostObject c ] "Sub Main()\n    Dim i\n    For i = 1 To 1000\n        C.X = C.X + 1\n    Next\n    Debug.Print C.X\nEnd Sub\n"
        if out <> [ " 1000 " ] then fail "count" [ " 1000 " ] out
        // (Three places in the code name X: the read in the loop, the write, the print.)
        if c.Resolved > 3 then fail "lookups" "at most 3" c.Resolved)

    "TryState finds the state of the table's own objects", (fun () ->
        let b = { Left = 1.0; Text = "a"; Items = ResizeArray() }
        match boxType.TryState (boxType.Wrap b) with
        | Some found when obj.ReferenceEquals(found, b) -> ()
        | other -> fail "TryState" "the box" other
        if (boxType.TryState (VString "x")).IsSome then fail "TryState of a string" None "Some"
        if not (List.contains "Caption" boxType.Names) then fail "Names" "Caption" boxType.Names)
]

let runAll () =
    let mutable failed = []
    for name, test in groups do
        try
            test ()
            printfn "PASS %s" name
        with error ->
            failed <- name :: failed
            eprintfn "FAIL %s: %s" name error.Message
    if not failed.IsEmpty then failwithf "%d of %d 0.6.0 groups failed: %s" failed.Length groups.Length (String.concat ", " (List.rev failed))
