module Rbt.Vb.Validator

open Ast

type ValidationError = {
    Message: string
}

let private error msg = { Message = msg }

let validateVBScript (program: Program) : ValidationError list =
    let errors = ResizeArray<ValidationError>()

    let checkParam (p: Parameter) =
        if p.Type.IsSome then
            errors.Add(error "VBScript does not allow typed parameters")
        if p.Optional then
            errors.Add(error (sprintf "VBScript does not allow Optional parameters (parameter '%s')" p.Name))
        if p.IsParamArray then
            errors.Add(error (sprintf "VBScript does not allow ParamArray (parameter '%s')" p.Name))
        if p.DefaultValue.IsSome then
            errors.Add(error (sprintf "VBScript does not allow default parameter values (parameter '%s')" p.Name))

    let checkModifiers (mods: Modifier list) =
        for m in mods do
            match m with
            | VisibilityMod Friend ->
                errors.Add(error "VBScript does not support the 'Friend' modifier")
            | StaticMod ->
                errors.Add(error "VBScript does not support the 'Static' modifier")
            | _ -> ()

    let checkVarDeclarator (d: VarDeclarator) =
        if d.Type.IsSome then
            errors.Add(error (sprintf "VBScript does not allow typed variable declarations (variable '%s')" d.Name))

    let rec checkFunction (f: Function) =
        match f with
        | FunctionDecl (mods, _, parms, returnType, body) ->
            checkModifiers mods
            for p in parms do checkParam p
            if returnType.IsSome then
                errors.Add(error "VBScript does not allow typed function return values")
            for s in body do checkStatement s
        | SubDecl (mods, _, parms, body) ->
            checkModifiers mods
            for p in parms do checkParam p
            for s in body do checkStatement s
        | PropertyGet (mods, _, parms, returnType, body) ->
            checkModifiers mods
            for p in parms do checkParam p
            if returnType.IsSome then
                errors.Add(error "VBScript does not allow typed property return values")
            for s in body do checkStatement s
        | PropertyLet (mods, _, parms, body)
        | PropertySet (mods, _, parms, body) ->
            checkModifiers mods
            for p in parms do checkParam p
            for s in body do checkStatement s

    and checkStatement (s: Statement) =
        match s with
        | Declaration (vis, Dim declarators) ->
            match vis with
            | Some Friend -> errors.Add(error "VBScript does not support the 'Friend' modifier on declarations")
            | _ -> ()
            for d in declarators do checkVarDeclarator d
        | Declaration (_, Const (_, typeRef, _)) ->
            if typeRef.IsSome then
                errors.Add(error "VBScript does not allow typed Const declarations")
        | Declaration (_, ReDim _) -> ()
        | OnError (GoToLabel _) ->
            errors.Add(error "VBScript does not support 'On Error GoTo <label>'")
        | IfStmt (_, thenStmts, elseIfs, elseStmts) ->
            for s in thenStmts do checkStatement s
            for (_, stmts) in elseIfs do
                for s in stmts do checkStatement s
            match elseStmts with
            | Some stmts -> for s in stmts do checkStatement s
            | None -> ()
        | SelectCase (_, cases) ->
            for (_, stmts) in cases do
                for s in stmts do checkStatement s
        | ForLoop (_, _, _, _, body) -> for s in body do checkStatement s
        | ForEach (_, _, body) -> for s in body do checkStatement s
        | WhileLoop (_, body) -> for s in body do checkStatement s
        | DoLoop (condOpt, bodyOpt) ->
            match condOpt with
            | Some dc -> for s in dc.Body do checkStatement s
            | None -> ()
            match bodyOpt with
            | Some dc -> for s in dc.Body do checkStatement s
            | None -> ()
        | WithStmt (_, body) -> for s in body do checkStatement s
        | GoToStmt _ ->
            errors.Add(error "VBScript does not support GoTo")
        | GoSubStmt _ ->
            errors.Add(error "VBScript does not support GoSub")
        | ReturnStmt ->
            errors.Add(error "VBScript does not support Return")
        | LabelStmt _ ->
            errors.Add(error "VBScript does not support labels")
        | _ -> ()

    and checkTopLevel (tl: TopLevel) =
        match tl with
        | FunctionDef f -> checkFunction f
        | ClassDecl (_, _, members) ->
            for m in members do checkTopLevel m
        | EnumDecl _ ->
            errors.Add(error "VBScript does not support Enum declarations")
        | TypeDecl _ ->
            errors.Add(error "VBScript does not support Type declarations")
        | DeclareDecl _ ->
            errors.Add(error "VBScript does not support Declare statements")
        | ImplementsDecl _ ->
            errors.Add(error "VBScript does not support Implements")
        | EventDecl (_, name, _) ->
            errors.Add(error (sprintf "VBScript does not support Event declarations (event '%s')" name))
        | WithEventsDecl (_, name, _) ->
            errors.Add(error (sprintf "VBScript does not support WithEvents (variable '%s')" name))
        | OptionStmt _ -> ()
        | TopLevelStatement s -> checkStatement s

    for tl in program.TopLevels do
        checkTopLevel tl

    errors |> Seq.toList

let validate (program: Program) : ValidationError list =
    match program.Dialect with
    | VBScript -> validateVBScript program
    | VBA -> []  // VBA validation is permissive for now
