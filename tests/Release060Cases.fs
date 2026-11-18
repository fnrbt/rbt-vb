module Rbt.Vb.Tests.Release060Cases

// 0.6.0: HostMembers - an object model as tables of members. Shared by .NET and Fable.

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

    "an unknown member is error 438 naming the type and member", (fun () ->
        let b = { Left = 1.0; Text = "a"; Items = ResizeArray() }
        let out = printed b "Sub Main()\n    On Error Resume Next\n    b.Nope = 1\n    Debug.Print Err.Number; Err.Description\n    Err.Clear\n    x = b.Nope2\n    Debug.Print Err.Description\nEnd Sub\n"
        let expected = [ " 438 Object doesn't support this property or method (Box.Nope)"; "Object doesn't support this property or method (Box.Nope2)" ]
        if out <> expected then fail "438" expected out)

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
