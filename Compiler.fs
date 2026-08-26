module Rbt.Vb.Compiler

open Ast
open Bytecode
open Values
open System.Collections.Generic

let private normalizeName (name: string) =
#if FABLE_COMPILER
    if isNull (box name) then "" else name.ToLowerInvariant()
#else
    name
#endif

let private newNameDictionary<'T> () =
#if FABLE_COMPILER
    Dictionary<string, 'T>()
#else
    Dictionary<string, 'T>(System.StringComparer.OrdinalIgnoreCase)
#endif

let private newNameSet () =
#if FABLE_COMPILER
    HashSet<string>()
#else
    HashSet<string>(System.StringComparer.OrdinalIgnoreCase)
#endif

let private containsName (dictionary: Dictionary<string, 'T>) name =
    dictionary.ContainsKey(normalizeName name)

let private tryGetName (dictionary: Dictionary<string, 'T>) name =
    dictionary.TryGetValue(normalizeName name)

let private getName (dictionary: Dictionary<string, 'T>) name =
    dictionary.[normalizeName name]

let private setName (dictionary: Dictionary<string, 'T>) name value =
    dictionary.[normalizeName name] <- value

let private containsSetName (set: HashSet<string>) name =
    set.Contains(normalizeName name)

let private addSetName (set: HashSet<string>) name =
    set.Add(normalizeName name)

let private copyNameDictionary (source: Dictionary<string, 'T>) =
    let copy = newNameDictionary ()
    for kv in source do
        setName copy kv.Key kv.Value
    copy

type CompileContext = {
    Instructions: ResizeArray<Instruction>
    Constants: ResizeArray<Value>
    ConstantIndex: Dictionary<Value, int>
    Globals: Dictionary<string, int>
    mutable Locals: Dictionary<string, int>
    Functions: ResizeArray<FunctionDef>
    FunctionIndex: Dictionary<string, int>
    Classes: ResizeArray<ClassDef>
    ClassIndex: Dictionary<string, int>
    ArrayVars: HashSet<string>
    mutable ClassFields: HashSet<string> option
    ExitLoopPatches: ResizeArray<ResizeArray<int>>
    Labels: Dictionary<string, int>
    LabelPatches: ResizeArray<string * int>
    mutable NextGlobalSlot: int
    mutable NextLocalSlot: int
    mutable ReturnSlotName: string option
    mutable InFunction: bool
    mutable WithTargetSlot: (bool * int) option
    ByRefInfo: Dictionary<string, bool array>  // function name -> which params are ByRef  // (isGlobal, slot)

}

let emptyContext () =
    {
        Instructions = ResizeArray()
        Constants = ResizeArray()
        ConstantIndex = Dictionary<Value, int>()
        Globals = newNameDictionary ()
        Locals = newNameDictionary ()
        Functions = ResizeArray()
        FunctionIndex = newNameDictionary ()
        Classes = ResizeArray()
        ClassIndex = newNameDictionary ()
        ArrayVars = newNameSet ()
        ClassFields = None
        ExitLoopPatches = ResizeArray()
        Labels = newNameDictionary ()
        LabelPatches = ResizeArray()
        NextGlobalSlot = 0
        NextLocalSlot = 0
        ReturnSlotName = None
        InFunction = false
        WithTargetSlot = None
        ByRefInfo = newNameDictionary ()
    }

let emit (ctx: CompileContext) (opcode: Opcode) (line: int) =
    ctx.Instructions.Add({ Opcode = opcode; LineNumber = line })

let patchJump (ctx: CompileContext) (pc: int) (opcode: Opcode) =
    ctx.Instructions.[pc] <- { ctx.Instructions.[pc] with Opcode = opcode }

let addConstant (ctx: CompileContext) (value: Value) =
    let mutable foundIndex = 0
    if ctx.ConstantIndex.TryGetValue(value, &foundIndex) then
        foundIndex
    else
        let index = ctx.Constants.Count
        ctx.Constants.Add(value)
        ctx.ConstantIndex.[value] <- index
        index

let getGlobalSlot (ctx: CompileContext) (name: string) =
    match tryGetName ctx.Globals name with
    | true, slot -> slot
    | false, _ ->
        setName ctx.Globals name ctx.NextGlobalSlot
        let slot = ctx.NextGlobalSlot
        ctx.NextGlobalSlot <- ctx.NextGlobalSlot + 1
        slot

let getLocalSlot (ctx: CompileContext) (name: string) =
    match tryGetName ctx.Locals name with
    | true, slot -> slot
    | false, _ ->
        setName ctx.Locals name ctx.NextLocalSlot
        let slot = ctx.NextLocalSlot
        ctx.NextLocalSlot <- ctx.NextLocalSlot + 1
        slot

let emitExitLoop (ctx: CompileContext) =
    let jumpPc = ctx.Instructions.Count
    emit ctx (Jump -1) 0
    if ctx.ExitLoopPatches.Count > 0 then
        ctx.ExitLoopPatches.[ctx.ExitLoopPatches.Count - 1].Add(jumpPc)

let patchExitLoopTargets (ctx: CompileContext) =
    if ctx.ExitLoopPatches.Count > 0 then
        let patches = ctx.ExitLoopPatches.[ctx.ExitLoopPatches.Count - 1]
        ctx.ExitLoopPatches.RemoveAt(ctx.ExitLoopPatches.Count - 1)
        let target = ctx.Instructions.Count
        for pc in patches do
            patchJump ctx pc (Jump target)

let emitCall (ctx: CompileContext) (name: string) (args: Expression list) (compileExpr: CompileContext -> Expression -> unit) =
    match tryGetName ctx.FunctionIndex name with
    | true, funcIndex ->
        // Look up ByRef info for the function
        let byRefInfo =
            match tryGetName ctx.ByRefInfo name with
            | true, arr -> arr
            | false, _ -> [||]
        for i in List.length args - 1 .. -1 .. 0 do
            let arg = List.item i args
            let isByRef = i < byRefInfo.Length && byRefInfo.[i]
            match isByRef, arg with
            | true, Identifier varName ->
                // Pass by reference: emit MakeRef instead of LoadLocal/LoadGlobal
                if containsName ctx.Globals varName && not (ctx.InFunction && containsName ctx.Locals varName) then
                    emit ctx (MakeRefGlobal (getName ctx.Globals varName)) 0
                elif containsName ctx.Locals varName then
                    emit ctx (MakeRefLocal (getName ctx.Locals varName)) 0
                else
                    compileExpr ctx arg  // fallback: pass by value
            | _ ->
                compileExpr ctx arg  // pass by value
        emit ctx (Call (funcIndex, List.length args)) 0
    | false, _ ->
        for i in List.length args - 1 .. -1 .. 0 do
            compileExpr ctx (List.item i args)
        // Builtins dispatch case-insensitively; lowercase the name once here so
        // the VM needn't re-lowercase (allocate) on every call.
        emit ctx (CallBuiltin (name.ToLowerInvariant(), List.length args)) 0

let private emitDefaultCall (ctx: CompileContext) (args: Expression list) (compileExpr: CompileContext -> Expression -> unit) =
    for i in List.length args - 1 .. -1 .. 0 do
        compileExpr ctx (List.item i args)
    emit ctx (CallDefault (List.length args)) 0

let rec compileExpression (ctx: CompileContext) (expr: Expression) =
    match expr with
    | Literal (Integer i) ->
        let idx = addConstant ctx (Value.VInteger i)
        emit ctx (LoadConst idx) 0
    | Literal (Double d) ->
        let idx = addConstant ctx (Value.VDouble d)
        emit ctx (LoadConst idx) 0
    | Literal (String s) ->
        let idx = addConstant ctx (Value.VString s)
        emit ctx (LoadConst idx) 0
    | Literal (Boolean b) ->
        if b then emit ctx LoadTrue 0 else emit ctx LoadFalse 0
    | Literal Null ->
        emit ctx LoadNull 0
    | Literal Empty ->
        emit ctx LoadEmpty 0
    | Literal Nothing ->
        let idx = addConstant ctx VNothing
        emit ctx (LoadConst idx) 0
    | Identifier name ->
        match ctx.ClassFields with
        | Some fields when containsSetName fields name ->
            emit ctx (LoadLocal 0) 0
            emit ctx (GetMember name) 0
        | _ ->
            if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                let slot = getName ctx.Globals name
                emit ctx (LoadGlobal slot) 0
            else
                let slot = getLocalSlot ctx name
                emit ctx (LoadLocal slot) 0
    | Unary (Ast.Not, e) ->
        compileExpression ctx e
        emit ctx Bytecode.Not 0
    | Unary (Ast.Negate, e) ->
        compileExpression ctx e
        emit ctx Bytecode.Negate 0
    | Binary (op, left, right) ->
        compileExpression ctx left
        compileExpression ctx right
        match op with
        | Ast.Add -> emit ctx Add 0
        | Ast.Subtract -> emit ctx Subtract 0
        | Ast.Multiply -> emit ctx Multiply 0
        | Ast.Divide -> emit ctx Divide 0
        | Ast.IntDivide -> emit ctx Bytecode.IntDivide 0
        | Ast.Modulus -> emit ctx Modulus 0
        | Ast.Power -> emit ctx Power 0
        | Ast.Concatenate -> emit ctx Concat 0
        | Ast.Equal -> emit ctx Equal 0
        | Ast.NotEqual -> emit ctx NotEqual 0
        | Ast.LessThan -> emit ctx LessThan 0
        | Ast.LessEqual -> emit ctx LessEqual 0
        | Ast.GreaterThan -> emit ctx GreaterThan 0
        | Ast.GreaterEqual -> emit ctx GreaterEqual 0
        | Ast.And -> emit ctx And 0
        | Ast.Or -> emit ctx Or 0
        | Ast.Xor -> emit ctx Xor 0
        | Ast.Is -> emit ctx IsOp 0
        | Ast.Like -> emit ctx LikeOp 0
        | Ast.Eqv -> emit ctx Bytecode.Eqv 0
        | Ast.Imp -> emit ctx Bytecode.Imp 0
    | Ast.Call (Identifier name, args) ->
        match ctx.ClassFields with
        | Some fields when containsSetName fields name ->
            emit ctx (LoadLocal 0) 0
            emit ctx (GetMember name) 0
            emitDefaultCall ctx args compileExpression
        | _ ->
            let isKnownValue =
                containsSetName ctx.ArrayVars name ||
                containsName ctx.Locals name ||
                containsName ctx.Globals name
            if isKnownValue && not (containsName ctx.FunctionIndex name) then
                if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                    emit ctx (LoadGlobal (getName ctx.Globals name)) 0
                else
                    emit ctx (LoadLocal (getLocalSlot ctx name)) 0
                emitDefaultCall ctx args compileExpression
            else
                emitCall ctx name args compileExpression
    | Ast.Call (Member (Identifier objName, methodName), args)
        when objName.ToLower() = "wscript" ->
        match methodName.ToLower() with
        | "echo" ->
            for i in List.length args - 1 .. -1 .. 0 do
                compileExpression ctx (List.item i args)
            emit ctx (CallBuiltin ("print", List.length args)) 0
        | "quit" -> emit ctx Halt 0
        | _ -> emit ctx LoadEmpty 0
    | Ast.Call (Member (Identifier objName, methodName), args)
        when objName.ToLower() = "err" ->
        match methodName.ToLower() with
        | "clear" -> emit ctx ClearErr 0
        | "raise" ->
            if args.Length >= 1 then compileExpression ctx (List.item 0 args)
            else emit ctx (LoadConst (addConstant ctx (VInteger 0))) 0
            if args.Length >= 2 then compileExpression ctx (List.item 1 args)
            else emit ctx (LoadConst (addConstant ctx (VString ""))) 0
            emit ctx RaiseErr 0
        | _ -> emit ctx LoadEmpty 0
    | Ast.Call (Member (objExpr, methodName), args) ->
        compileExpression ctx objExpr
        for i in List.length args - 1 .. -1 .. 0 do
            compileExpression ctx (List.item i args)
        emit ctx (CallMethod (methodName, List.length args)) 0
    | Ast.Call (_, args) ->
        for i in List.length args - 1 .. -1 .. 0 do
            compileExpression ctx (List.item i args)
        emit ctx LoadNull 0
    | MeExpr ->
        // Slot 0 in class methods is reserved for Me
        emit ctx (LoadLocal 0) 0
    | TypeOfIs (expr, className) ->
        compileExpression ctx expr
        emit ctx (TypeCheck className) 0
    | NewExpr className ->
        match tryGetName ctx.ClassIndex className with
        | true, idx -> emit ctx (NewObj idx) 0
        | false, _ -> emit ctx LoadNull 0
    | WithDotExpr ->
        match ctx.WithTargetSlot with
        | Some (true, slot) -> emit ctx (LoadGlobal slot) 0
        | Some (false, slot) -> emit ctx (LoadLocal slot) 0
        | None -> emit ctx LoadEmpty 0
    | Member (Identifier objName, memberName) when objName.ToLower() = "err" ->
        match memberName.ToLower() with
        | "number" -> emit ctx LoadErrNumber 0
        | "description" -> emit ctx LoadErrDescription 0
        | "source" -> emit ctx (LoadConst (addConstant ctx (VString ""))) 0
        | "clear" -> emit ctx ClearErr 0
        | _ -> emit ctx LoadEmpty 0
    | Member (objExpr, memberName) ->
        compileExpression ctx objExpr
        emit ctx (GetMember memberName) 0

let rec compileStatement (ctx: CompileContext) (stmt: Statement) =
    match stmt with
    | ExpressionStmt expr ->
        compileExpression ctx expr
        emit ctx Pop 0

    | Assignment (name, expr) ->
        match ctx.ClassFields with
        | Some fields when containsSetName fields name ->
            emit ctx (LoadLocal 0) 0
            compileExpression ctx expr
            emit ctx (SetMember name) 0
        | _ ->
            compileExpression ctx expr
            if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                let slot = getName ctx.Globals name
                emit ctx (StoreGlobal slot) 0
            else
                let slot = getLocalSlot ctx name
                emit ctx (StoreLocal slot) 0

    | IndexedAssignment (name, indices, valueExpr) ->
        match ctx.ClassFields with
        | Some fields when containsSetName fields name ->
            emit ctx (LoadLocal 0) 0
            emit ctx (GetMember name) 0
            compileExpression ctx (List.head indices)
            compileExpression ctx valueExpr
            emit ctx ArraySet 0
        | _ ->
            if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                emit ctx (LoadGlobal (getName ctx.Globals name)) 0
            else
                emit ctx (LoadLocal (getLocalSlot ctx name)) 0
            compileExpression ctx (List.head indices)
            compileExpression ctx valueExpr
            emit ctx ArraySet 0

    | MemberAssignment (objExpr, memberName, valueExpr) ->
        compileExpression ctx objExpr
        compileExpression ctx valueExpr
        emit ctx (SetMember memberName) 0

    | Ast.Set (name, expr) ->
        match ctx.ClassFields with
        | Some fields when containsSetName fields name ->
            emit ctx (LoadLocal 0) 0
            compileExpression ctx expr
            emit ctx (SetMember name) 0
        | _ ->
            compileExpression ctx expr
            if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                emit ctx (StoreGlobal (getName ctx.Globals name)) 0
            else
                let slot = getLocalSlot ctx name
                emit ctx (StoreLocal slot) 0

    | Declaration (_, Dim declarators) ->
        for d in declarators do
            if not ctx.InFunction && ctx.ClassFields.IsNone then
                // Top level: use globals
                let gslot = getGlobalSlot ctx d.Name
                match d.ArraySpec with
                | Some spec when spec.Dimensions.Length > 0 ->
                    compileExpression ctx (List.head spec.Dimensions)
                    emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
                    emit ctx Add 0
                    emit ctx ArrayNew 0
                    emit ctx (StoreGlobal gslot) 0
                    addSetName ctx.ArrayVars d.Name |> ignore
                | _ ->
                    emit ctx LoadEmpty 0
                    emit ctx (StoreGlobal gslot) 0
            else
                let slot = getLocalSlot ctx d.Name
                match d.ArraySpec with
                | Some spec when spec.Dimensions.Length > 0 ->
                    compileExpression ctx (List.head spec.Dimensions)
                    emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
                    emit ctx Add 0
                    emit ctx ArrayNew 0
                    emit ctx (StoreLocal slot) 0
                    addSetName ctx.ArrayVars d.Name |> ignore
                | _ ->
                    emit ctx LoadEmpty 0
                    emit ctx (StoreLocal slot) 0

    | Declaration (_, Const (name, _, expr)) ->
        compileExpression ctx expr
        if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
            emit ctx (StoreGlobal (getName ctx.Globals name)) 0
        else
            let slot = getLocalSlot ctx name
            emit ctx (StoreLocal slot) 0

    | Declaration (_, ReDim (name, sizeExpr, preserve)) ->
        match ctx.ClassFields with
        | Some fields when containsSetName fields name ->
            // ReDim on a class field — store via Me
            if preserve then
                emit ctx (LoadLocal 0) 0
                emit ctx (GetMember name) 0
                compileExpression ctx sizeExpr
                emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
                emit ctx Add 0
                emit ctx ReDimPreserve 0
                // Store back: LoadLocal 0 (Me), swap, SetMember
                // Simpler: just use a temp
                let tmpSlot = getLocalSlot ctx "__redim_tmp__"
                emit ctx (StoreLocal tmpSlot) 0
                emit ctx (LoadLocal 0) 0
                emit ctx (LoadLocal tmpSlot) 0
                emit ctx (SetMember name) 0
            else
                emit ctx (LoadLocal 0) 0
                compileExpression ctx sizeExpr
                emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
                emit ctx Add 0
                emit ctx ArrayNew 0
                emit ctx (SetMember name) 0
        | _ ->
            let useGlobal = containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name)
            if preserve then
                if useGlobal then emit ctx (LoadGlobal (getName ctx.Globals name)) 0
                else emit ctx (LoadLocal (getLocalSlot ctx name)) 0
                compileExpression ctx sizeExpr
                emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
                emit ctx Add 0
                emit ctx ReDimPreserve 0
                if useGlobal then emit ctx (StoreGlobal (getName ctx.Globals name)) 0
                else emit ctx (StoreLocal (getLocalSlot ctx name)) 0
            else
                compileExpression ctx sizeExpr
                emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
                emit ctx Add 0
                emit ctx ArrayNew 0
                if useGlobal then emit ctx (StoreGlobal (getName ctx.Globals name)) 0
                else emit ctx (StoreLocal (getLocalSlot ctx name)) 0
            addSetName ctx.ArrayVars name |> ignore

    | IfStmt (condition, thenStmts, elseIfs, elseStmtsOpt) ->
        compileExpression ctx condition
        let elseJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0

        for stmt in thenStmts do
            compileStatement ctx stmt

        let endJumps = ResizeArray<int>()

        for (elseIfCond, elseIfBody) in elseIfs do
            let jumpToEndPc = ctx.Instructions.Count
            emit ctx (Jump -1) 0
            endJumps.Add(jumpToEndPc)

            patchJump ctx elseJumpPc (JumpIfFalse ctx.Instructions.Count)

            compileExpression ctx elseIfCond
            let nextJumpPc = ctx.Instructions.Count
            emit ctx (JumpIfFalse -1) 0

            for stmt in elseIfBody do
                compileStatement ctx stmt

            let jumpToEnd2 = ctx.Instructions.Count
            emit ctx (Jump -1) 0
            endJumps.Add(jumpToEnd2)

            patchJump ctx nextJumpPc (JumpIfFalse ctx.Instructions.Count)

        match elseStmtsOpt with
        | Some elseStmts when elseStmts.Length > 0 ->
            if elseIfs.IsEmpty then
                let endJumpPc = ctx.Instructions.Count
                emit ctx (Jump -1) 0
                endJumps.Add(endJumpPc)
                patchJump ctx elseJumpPc (JumpIfFalse ctx.Instructions.Count)

            for stmt in elseStmts do
                compileStatement ctx stmt

        | _ ->
            if elseIfs.IsEmpty then
                patchJump ctx elseJumpPc (JumpIfFalse ctx.Instructions.Count)

        let finalTarget = ctx.Instructions.Count
        for jumpPc in endJumps do
            patchJump ctx jumpPc (Jump finalTarget)

    | ForLoop (varName, startExpr, endExpr, stepOpt, body) ->
        let isNegativeStep =
            match stepOpt with
            | Some (Ast.Unary (Ast.Negate, Literal (Integer _))) -> true
            | Some (Ast.Unary (Ast.Negate, Literal (Double _))) -> true
            | Some (Literal (Integer n)) when n < 0 -> true
            | Some (Literal (Double d)) when d < 0.0 -> true
            | _ -> false

        // Use same storage as Identifier/Assignment for the loop variable
        let useGlobal = containsName ctx.Globals varName && not (ctx.InFunction && containsName ctx.Locals varName)
        compileExpression ctx startExpr
        if useGlobal then
            emit ctx (StoreGlobal (getName ctx.Globals varName)) 0
        else
            let slot = getLocalSlot ctx varName
            emit ctx (StoreLocal slot) 0

        let loopStartPc = ctx.Instructions.Count

        compileExpression ctx (Identifier varName)
        compileExpression ctx endExpr
        if isNegativeStep then
            emit ctx LessThan 0
        else
            emit ctx GreaterThan 0
        let exitJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfTrue -1) 0

        ctx.ExitLoopPatches.Add(ResizeArray())

        for stmt in body do
            compileStatement ctx stmt

        compileExpression ctx (Identifier varName)
        match stepOpt with
        | Some stepExpr ->
            compileExpression ctx stepExpr
        | None ->
            emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
        emit ctx Add 0
        if useGlobal then
            emit ctx (StoreGlobal (getName ctx.Globals varName)) 0
        else
            let slot = getLocalSlot ctx varName
            emit ctx (StoreLocal slot) 0

        emit ctx (Jump loopStartPc) 0

        patchJump ctx exitJumpPc (JumpIfTrue ctx.Instructions.Count)
        patchExitLoopTargets ctx

    | ForEach (varName, collExpr, body) ->
        compileExpression ctx collExpr
        let collSlot = getLocalSlot ctx "__foreach_coll__"
        emit ctx (StoreLocal collSlot) 0
        let idxSlot = getLocalSlot ctx "__foreach_idx__"
        emit ctx (LoadConst (addConstant ctx (VInteger 0))) 0
        emit ctx (StoreLocal idxSlot) 0

        let loopStartPc = ctx.Instructions.Count

        emit ctx (LoadLocal idxSlot) 0
        emit ctx (LoadLocal collSlot) 0
        emit ctx ArrayLength 0
        emit ctx GreaterEqual 0
        let exitJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfTrue -1) 0

        emit ctx (LoadLocal collSlot) 0
        emit ctx (LoadLocal idxSlot) 0
        emit ctx ArrayGet 0
        let varSlot = getLocalSlot ctx varName
        emit ctx (StoreLocal varSlot) 0

        ctx.ExitLoopPatches.Add(ResizeArray())

        for stmt in body do
            compileStatement ctx stmt

        emit ctx (LoadLocal idxSlot) 0
        emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
        emit ctx Add 0
        emit ctx (StoreLocal idxSlot) 0

        emit ctx (Jump loopStartPc) 0

        patchJump ctx exitJumpPc (JumpIfTrue ctx.Instructions.Count)
        patchExitLoopTargets ctx

    | WhileLoop (condition, body) ->
        let loopStartPc = ctx.Instructions.Count
        compileExpression ctx condition
        let exitJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0

        ctx.ExitLoopPatches.Add(ResizeArray())

        for stmt in body do
            compileStatement ctx stmt

        emit ctx (Jump loopStartPc) 0

        patchJump ctx exitJumpPc (JumpIfFalse ctx.Instructions.Count)
        patchExitLoopTargets ctx

    | DoLoop (condOpt, bodyOpt) ->
        ctx.ExitLoopPatches.Add(ResizeArray())

        match condOpt, bodyOpt with
        | Some dc, None ->
            let loopStartPc = ctx.Instructions.Count
            compileExpression ctx dc.Condition
            let exitJumpPc = ctx.Instructions.Count
            emit ctx (JumpIfFalse -1) 0

            for stmt in dc.Body do
                compileStatement ctx stmt

            emit ctx (Jump loopStartPc) 0

            patchJump ctx exitJumpPc (JumpIfFalse ctx.Instructions.Count)

        | None, Some dc ->
            let loopStartPc = ctx.Instructions.Count

            for stmt in dc.Body do
                compileStatement ctx stmt

            compileExpression ctx dc.Condition
            emit ctx (JumpIfTrue loopStartPc) 0

        | _ ->
            ()

        patchExitLoopTargets ctx

    | SelectCase (expr, cases) ->
        compileExpression ctx expr
        let selectIsGlobal = containsName ctx.Globals "__select_temp__" || (not ctx.InFunction && ctx.ClassFields.IsNone)
        if selectIsGlobal then
            let gslot = getGlobalSlot ctx "__select_temp__"
            emit ctx (StoreGlobal gslot) 0
        else
            let slot = getLocalSlot ctx "__select_temp__"
            emit ctx (StoreLocal slot) 0

        let loadTemp () =
            if selectIsGlobal then emit ctx (LoadGlobal (getName ctx.Globals "__select_temp__")) 0
            else emit ctx (LoadLocal (getName ctx.Locals "__select_temp__")) 0

        let endJumps = ResizeArray<int>()

        for (caseTests, caseBody) in cases do
            match caseTests with
            | None ->
                for stmt in caseBody do
                    compileStatement ctx stmt
            | Some tests ->
                let mutable firstTest = true
                for test in tests do
                    match test with
                    | CaseValue expr ->
                        loadTemp ()
                        compileExpression ctx expr
                        emit ctx Equal 0
                    | CaseRange (lo, hi) ->
                        loadTemp ()
                        compileExpression ctx lo
                        emit ctx GreaterEqual 0
                        loadTemp ()
                        compileExpression ctx hi
                        emit ctx LessEqual 0
                        emit ctx And 0
                    | CaseComparison (op, expr) ->
                        loadTemp ()
                        compileExpression ctx expr
                        match op with
                        | Ast.LessThan -> emit ctx LessThan 0
                        | Ast.LessEqual -> emit ctx LessEqual 0
                        | Ast.GreaterThan -> emit ctx GreaterThan 0
                        | Ast.GreaterEqual -> emit ctx GreaterEqual 0
                        | Ast.NotEqual -> emit ctx NotEqual 0
                        | _ -> emit ctx Equal 0
                    if not firstTest then
                        emit ctx Or 0
                    firstTest <- false

                let skipPc = ctx.Instructions.Count
                emit ctx (JumpIfFalse -1) 0

                for stmt in caseBody do
                    compileStatement ctx stmt

                let endJumpPc = ctx.Instructions.Count
                emit ctx (Jump -1) 0
                endJumps.Add(endJumpPc)

                patchJump ctx skipPc (JumpIfFalse ctx.Instructions.Count)

        let finalTarget = ctx.Instructions.Count
        for jumpPc in endJumps do
            patchJump ctx jumpPc (Jump finalTarget)

    | ExitFor | ExitDo ->
        emitExitLoop ctx

    | ExitSub | ExitFunction | ExitProperty ->
        match ctx.ReturnSlotName with
        | Some name ->
            let retSlot = getName ctx.Locals name
            emit ctx (LoadLocal retSlot) 0
            emit ctx Return 0
        | None -> ()

    | CallStmt (Identifier name, args) ->
        emitCall ctx name args compileExpression
        emit ctx Pop 0
    | CallStmt (expr, _) ->
        compileExpression ctx expr
        emit ctx Pop 0

    | GoToStmt label ->
        let jumpPc = ctx.Instructions.Count
        emit ctx (Jump -1) 0
        ctx.LabelPatches.Add(label, jumpPc)

    | GoSubStmt label ->
        let jumpPc = ctx.Instructions.Count
        emit ctx (Bytecode.GoSub -1) 0
        ctx.LabelPatches.Add(label, jumpPc)

    | ReturnStmt ->
        emit ctx ReturnSub 0

    | LabelStmt label ->
        setName ctx.Labels label ctx.Instructions.Count

    | OnError ResumeNext ->
        emit ctx OnErrorResumeNext 0

    | OnError GoToZero ->
        emit ctx OnErrorGoToZero 0

    | OnError (GoToLabel label) ->
        let jumpPc = ctx.Instructions.Count
        emit ctx (OnErrorGoToLabel -1) 0
        ctx.LabelPatches.Add(label, jumpPc)

    | WithStmt (target, body) ->
        let outerWith = ctx.WithTargetSlot
        compileExpression ctx target
        let useGlobal = not ctx.InFunction
        if useGlobal then
            let gslot = getGlobalSlot ctx "__with__"
            emit ctx (StoreGlobal gslot) 0
            ctx.WithTargetSlot <- Some (true, gslot)
        else
            let slot = getLocalSlot ctx "__with__"
            emit ctx (StoreLocal slot) 0
            ctx.WithTargetSlot <- Some (false, slot)
        for stmt in body do
            compileStatement ctx stmt
        ctx.WithTargetSlot <- outerWith

    | EraseStmt name ->
        let slot = getLocalSlot ctx name
        emit ctx LoadEmpty 0
        emit ctx (StoreLocal slot) 0
        ctx.ArrayVars.Remove(normalizeName name) |> ignore

    | _ ->
        ()

let patchLabels (ctx: CompileContext) =
    for (label, pc) in ctx.LabelPatches do
        match tryGetName ctx.Labels label with
        | true, target ->
            let instr = ctx.Instructions.[pc]
            let newOpcode =
                match instr.Opcode with
                | Jump _ -> Jump target
                | Bytecode.GoSub _ -> Bytecode.GoSub target
                | OnErrorGoToLabel _ -> OnErrorGoToLabel target
                | other -> other
            patchJump ctx pc newOpcode
        | false, _ -> ()

let compileFunctionBody (ctx: CompileContext) (name: string) (returnName: string) (paramList: Parameter list) (body: Statement list) =
    let outerLocals = copyNameDictionary ctx.Locals
    let outerNextLocal = ctx.NextLocalSlot
    let outerReturnSlot = ctx.ReturnSlotName
    let outerClassFields = ctx.ClassFields
    let outerLabels = Dictionary(ctx.Labels)
    let outerPatches = ResizeArray(ctx.LabelPatches)

    ctx.Locals <- newNameDictionary ()
    ctx.NextLocalSlot <- 0
    ctx.ReturnSlotName <- Some returnName
    ctx.InFunction <- true
    ctx.Labels.Clear()
    ctx.LabelPatches.Clear()

    // Reserve slot 0 for Me if inside a class method
    if ctx.ClassFields.IsSome then
        ignore (getLocalSlot ctx "__me__")

    for param in paramList do
        ignore (getLocalSlot ctx param.Name)

    ignore (getLocalSlot ctx returnName)

    // Register function index BEFORE compiling body (enables recursion)
    let funcIndex = ctx.Functions.Count
    setName ctx.FunctionIndex name funcIndex
    // Also register the return name for recursive calls (e.g. "Foo" not "Class.Foo")
    if returnName <> name then
        setName ctx.FunctionIndex returnName funcIndex

    let startPc = ctx.Instructions.Count

    for stmt in body do
        compileStatement ctx stmt

    let retSlot = getName ctx.Locals returnName
    emit ctx (LoadLocal retSlot) 0
    emit ctx Return 0

    patchLabels ctx

    let localCount = ctx.NextLocalSlot

    ctx.Locals <- outerLocals
    ctx.NextLocalSlot <- outerNextLocal
    ctx.ReturnSlotName <- outerReturnSlot
    ctx.InFunction <- false
    ctx.ClassFields <- outerClassFields
    ctx.Labels.Clear()
    for kv in outerLabels do setName ctx.Labels kv.Key kv.Value
    ctx.LabelPatches.Clear()
    for p in outerPatches do ctx.LabelPatches.Add(p)

    let byRefArr = paramList |> List.map (fun p -> p.ByRef) |> List.toArray
    setName ctx.ByRefInfo name byRefArr
    if returnName <> name then setName ctx.ByRefInfo returnName byRefArr

    let funcDef = {
        Name = name
        ParamsCount = List.length paramList
        LocalsCount = localCount
        ByRefParams = byRefArr
        Code = [||]
        StartPC = startPc
    }
    ctx.Functions.Add(funcDef)

let rec compileFunctions (ctx: CompileContext) (topLevel: TopLevel) =
    match topLevel with
    | FunctionDef func ->
        match func with
        | FunctionDecl (_, name, paramList, _, body) -> compileFunctionBody ctx name name paramList body
        | SubDecl (_, name, paramList, body) -> compileFunctionBody ctx name name paramList body
        | PropertyGet (_, name, paramList, _, body) -> compileFunctionBody ctx (name + "_get") name paramList body
        | PropertyLet (_, name, paramList, body) -> compileFunctionBody ctx (name + "_let") name paramList body
        | PropertySet (_, name, paramList, body) -> compileFunctionBody ctx (name + "_set") name paramList body
    | ClassDecl (_, className, members) ->
        let mutable fields = []
        let mutable methods = Map.empty
        let mutable getters = Map.empty
        let mutable letters = Map.empty
        let mutable setters = Map.empty

        // First pass: collect field names
        for m in members do
            match m with
            | TopLevelStatement (Declaration (_, Dim declarators)) ->
                for d in declarators do
                    fields <- d.Name :: fields
            | _ -> ()

        let fieldSet = newNameSet ()
        for field in List.rev fields do
            addSetName fieldSet field |> ignore

        // Second pass: compile methods with field awareness
        for m in members do
            match m with
            | FunctionDef func ->
                ctx.ClassFields <- Some fieldSet
                match func with
                | SubDecl (_, name, paramList, body) ->
                    let fullName = className + "." + name
                    compileFunctionBody ctx fullName name paramList body
                    methods <- Map.add name (ctx.Functions.Count - 1) methods
                | FunctionDecl (_, name, paramList, _, body) ->
                    let fullName = className + "." + name
                    compileFunctionBody ctx fullName name paramList body
                    methods <- Map.add name (ctx.Functions.Count - 1) methods
                | PropertyGet (_, name, paramList, _, body) ->
                    let fullName = className + "." + name + "_get"
                    compileFunctionBody ctx fullName name paramList body
                    getters <- Map.add name (ctx.Functions.Count - 1) getters
                | PropertyLet (_, name, paramList, body) ->
                    let fullName = className + "." + name + "_let"
                    compileFunctionBody ctx fullName name paramList body
                    letters <- Map.add name (ctx.Functions.Count - 1) letters
                | PropertySet (_, name, paramList, body) ->
                    let fullName = className + "." + name + "_set"
                    compileFunctionBody ctx fullName name paramList body
                    setters <- Map.add name (ctx.Functions.Count - 1) setters
            | _ -> ()

        let classIdx = ctx.Classes.Count
        setName ctx.ClassIndex className classIdx
        ctx.Classes.Add({
            Name = className
            Fields = List.rev fields |> List.toArray
            Methods = methods
            PropertyGetters = getters
            PropertyLetters = letters
            PropertySetters = setters
        })
    | _ -> ()

let rec compileTopLevel (ctx: CompileContext) (topLevel: TopLevel) =
    match topLevel with
    | FunctionDef _ -> ()
    | ClassDecl _ -> ()
    | TopLevelStatement stmt ->
        compileStatement ctx stmt
    | EnumDecl _ -> ()
    | TypeDecl _ -> ()
    | DeclareDecl _ -> ()
    | ImplementsDecl _ -> ()
    | EventDecl _ -> ()
    | WithEventsDecl _ -> ()
    | OptionStmt _ -> ()

let rec preRegisterGlobals (ctx: CompileContext) (topLevel: TopLevel) =
    match topLevel with
    | TopLevelStatement (Declaration (_, Dim declarators)) ->
        for d in declarators do
            ignore (getGlobalSlot ctx d.Name)
    | TopLevelStatement (Declaration (_, Const (name, _, _))) ->
        ignore (getGlobalSlot ctx name)
    | TopLevelStatement (Declaration (_, ReDim (name, _, _))) ->
        ignore (getGlobalSlot ctx name)
    | _ -> ()

let private globalNames (ctx: CompileContext) =
    let names = Array.create ctx.NextGlobalSlot ""
    for kvp in ctx.Globals do
        names.[kvp.Value] <- kvp.Key
    names

let compileWithGlobalNames (names: string seq) (program: Program) : BytecodeProgram =
    let ctx = emptyContext ()

    for name in names do
        if not (System.String.IsNullOrWhiteSpace name) then
            ignore (getGlobalSlot ctx name)

    // Pass 0: pre-register top-level Dim/Const/ReDim as globals
    for tl in program.TopLevels do
        preRegisterGlobals ctx tl

    emit ctx (Jump -1) 0

    for tl in program.TopLevels do
        compileFunctions ctx tl

    patchJump ctx 0 (Jump ctx.Instructions.Count)

    for tl in program.TopLevels do
        compileTopLevel ctx tl

    patchLabels ctx

    emit ctx Halt 0

    {
        Code = ctx.Instructions.ToArray()
        Constants = ctx.Constants.ToArray()
        Globals = ctx.NextGlobalSlot
        GlobalNames = globalNames ctx
        Functions = ctx.Functions.ToArray()
        Classes = ctx.Classes.ToArray()
    }

let compile (program: Program) : BytecodeProgram =
    compileWithGlobalNames Seq.empty program
