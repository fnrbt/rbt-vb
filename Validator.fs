module Rbt.Vb.Validator

open Ast

type ValidationError = {
    Message: string
}

let private error msg = { Message = msg }

let validateVBScript (program: Program) : ValidationError list =
    let errors = ResizeArray<ValidationError>()

    let rec checkExpression expr =
        match expr with
        | TypeOfIs (operand, _) ->
            errors.Add(error "VBScript does not support TypeOf ... Is expressions")
            checkExpression operand
        | Parenthesized operand
        | Unary (_, operand) -> checkExpression operand
        | ArrayBound (lower, upper) ->
            errors.Add(error "VBScript does not support explicit lower array bounds")
            checkExpression lower
            checkExpression upper
        | Binary (_, lower, upper) ->
            checkExpression lower
            checkExpression upper
        | Call (target, args) ->
            checkExpression target
            for arg in args do checkExpression arg
        | Member (receiver, _) -> checkExpression receiver
        | _ -> ()

    let checkCaseTest test =
        match test with
        | CaseValue expr -> checkExpression expr
        | CaseRange (lower, upper) ->
            errors.Add(error "VBScript does not support Case ranges")
            checkExpression lower
            checkExpression upper
        | CaseComparison (_, expr) ->
            errors.Add(error "VBScript does not support Case Is comparisons")
            checkExpression expr

    let checkParam (p: Parameter) =
        if p.Type.IsSome then
            errors.Add(error "VBScript does not allow typed parameters")
        if p.Optional then
            errors.Add(error (sprintf "VBScript does not allow Optional parameters (parameter '%s')" p.Name))
        if p.IsParamArray then
            errors.Add(error (sprintf "VBScript does not allow ParamArray (parameter '%s')" p.Name))
        match p.DefaultValue with
        | Some value ->
            errors.Add(error (sprintf "VBScript does not allow default parameter values (parameter '%s')" p.Name))
            checkExpression value
        | None -> ()

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
        match d.ArraySpec with
        | Some spec -> for bound in spec.Dimensions do checkExpression bound
        | None -> ()

    let rec checkFunction (f: Function) =
        match f with
        | FunctionDecl (mods, _, parms, returnType, body)
        | PropertyGet (mods, _, parms, returnType, body) ->
            checkModifiers mods
            for p in parms do checkParam p
            if returnType.IsSome then
                errors.Add(error "VBScript does not allow typed function or property return values")
            for s in body do checkStatement s
        | SubDecl (mods, _, parms, body)
        | PropertyLet (mods, _, parms, body)
        | PropertySet (mods, _, parms, body) ->
            checkModifiers mods
            for p in parms do checkParam p
            for s in body do checkStatement s

    and checkStatement (s: Statement) =
        match s with
        | LocatedStmt (_, inner) -> checkStatement inner
        | ExpressionStmt expr
        | Let (_, expr)
        | Set (_, expr)
        | Assignment (_, expr) -> checkExpression expr
        | Declaration (vis, Dim declarators) ->
            match vis with
            | Some Friend -> errors.Add(error "VBScript does not support the 'Friend' modifier on declarations")
            | _ -> ()
            for d in declarators do checkVarDeclarator d
        | Declaration (_, Const (_, typeRef, expr)) ->
            if typeRef.IsSome then errors.Add(error "VBScript does not allow typed Const declarations")
            checkExpression expr
        | Declaration (_, ReDim (_, bounds, _)) ->
            for bound in bounds do checkExpression bound
        | OnError (GoToLabel _) ->
            errors.Add(error "VBScript does not support 'On Error GoTo <label>'")
        | IfStmt (cond, thenStmts, elseIfs, elseStmts) ->
            checkExpression cond
            for s in thenStmts do checkStatement s
            for (condition, stmts) in elseIfs do
                checkExpression condition
                for s in stmts do checkStatement s
            match elseStmts with
            | Some stmts -> for s in stmts do checkStatement s
            | None -> ()
        | SelectCase (selector, cases) ->
            checkExpression selector
            for (tests, stmts) in cases do
                match tests with
                | Some tests -> for test in tests do checkCaseTest test
                | None -> ()
                for s in stmts do checkStatement s
        | ForLoop (_, startValue, endValue, step, body) ->
            checkExpression startValue
            checkExpression endValue
            Option.iter checkExpression step
            for s in body do checkStatement s
        | ForEach (_, collection, body)
        | WhileLoop (collection, body)
        | WithStmt (collection, body) ->
            checkExpression collection
            for s in body do checkStatement s
        | DoLoop (pre, post) ->
            for cond in [pre; post] do
                match cond with
                | Some dc ->
                    checkExpression dc.Condition
                    for s in dc.Body do checkStatement s
                | None -> ()
        | IndexedAssignment (_, indices, value) ->
            for index in indices do checkExpression index
            checkExpression value
        | MemberAssignment (receiver, _, value, _) ->
            checkExpression receiver
            checkExpression value
        | CallStmt (target, arguments) ->
            checkExpression target
            for arg in arguments do checkExpression arg
        | GoToStmt _ -> errors.Add(error "VBScript does not support GoTo")
        | GoSubStmt _ -> errors.Add(error "VBScript does not support GoSub")
        | ReturnStmt -> errors.Add(error "VBScript does not support Return")
        | LabelStmt _ -> errors.Add(error "VBScript does not support labels")
        | _ -> ()
    and checkTopLevel (tl: TopLevel) =
        match tl with
        | FunctionDef f -> checkFunction f
        | ClassDecl (_, _, members) ->
            for m in members do checkTopLevel m
        | EnumDecl (_, _, members) ->
            errors.Add(error "VBScript does not support Enum declarations")
            for memberInfo in members do
                Option.iter checkExpression memberInfo.Value
        | TypeDecl (_, _, members) ->
            errors.Add(error "VBScript does not support Type declarations")
            for memberInfo in members do
                match memberInfo.ArraySpec with
                | Some spec -> for bound in spec.Dimensions do checkExpression bound
                | None -> ()
        | DeclareDecl (_, info) ->
            errors.Add(error "VBScript does not support Declare statements")
            for param in info.Parameters do checkParam param
        | ImplementsDecl _ ->
            errors.Add(error "VBScript does not support Implements")
        | EventDecl (_, name, parameters) ->
            errors.Add(error (sprintf "VBScript does not support Event declarations (event '%s')" name))
            for param in parameters do checkParam param
        | WithEventsDecl (_, name, _) ->
            errors.Add(error (sprintf "VBScript does not support WithEvents (variable '%s')" name))
        | OptionStmt optionName when not (optionName.Equals("explicit", System.StringComparison.OrdinalIgnoreCase)) ->
            errors.Add(error (sprintf "VBScript does not support Option %s" optionName))
        | OptionStmt _ -> ()
        | TopLevelStatement s -> checkStatement s

    for tl in program.TopLevels do
        checkTopLevel tl

    errors |> Seq.toList

let validate (program: Program) : ValidationError list =
    match program.Dialect with
    | VBScript -> validateVBScript program
    | VBA -> []  // VBA validation is permissive for now
