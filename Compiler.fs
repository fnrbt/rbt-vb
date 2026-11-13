module Rbt.Vb.Compiler

open Ast
open Bytecode
open Values
open System.Collections.Generic

let private normalizeName (name: string) =
#if FABLE_COMPILER
    if isNull (box name) then "" else name.ToLowerInvariant()
#else
    name.ToLowerInvariant()
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
    mutable ClassName: string option
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
        ClassName = None
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

let private freshLocal (ctx: CompileContext) =
    let slot = ctx.NextLocalSlot
    ctx.NextLocalSlot <- slot + 1
    slot

let private isField (ctx: CompileContext) name =
    not (containsName ctx.Locals name) &&
    (match ctx.ClassFields with Some fields -> containsSetName fields name | None -> false)

let private storeName (ctx: CompileContext) name =
    if isField ctx name then
        let temp = freshLocal ctx
        emit ctx (StoreLocal temp) 0
        emit ctx (LoadLocal 0) 0
        emit ctx (LoadLocal temp) 0
        emit ctx (SetMember (name, false)) 0
    elif containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
        emit ctx (StoreGlobal (getName ctx.Globals name)) 0
    else
        emit ctx (StoreLocal (getLocalSlot ctx name)) 0

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
            | true, Identifier varName when isField ctx varName ->
                emit ctx (LoadLocal 0) 0
                emit ctx (MakeRefMember varName) 0
            | true, Identifier varName ->
                // Pass by reference: emit MakeRef instead of LoadLocal/LoadGlobal
                if containsName ctx.Globals varName && not (ctx.InFunction && containsName ctx.Locals varName) then
                    emit ctx (MakeRefGlobal (getName ctx.Globals varName)) 0
                elif containsName ctx.Locals varName then
                    emit ctx (MakeRefLocal (getName ctx.Locals varName)) 0
                else
                    compileExpr ctx arg  // fallback: pass by value
            | true, Ast.Call (Identifier arrayName, indices) when containsSetName ctx.ArrayVars arrayName || containsName ctx.Locals arrayName || containsName ctx.Globals arrayName ->
                compileExpr ctx (Identifier arrayName)
                for index in indices do compileExpr ctx index
                emit ctx (MakeRefArray indices.Length) 0
            | true, Member (Identifier objectName, _) when objectName.ToLowerInvariant() = "err" ->
                compileExpr ctx arg
            | true, Member (receiver, memberName) ->
                compileExpr ctx receiver
                emit ctx (MakeRefMember memberName) 0
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
    | Parenthesized e -> compileExpression ctx e
    | ArrayBound _ -> failwith "Array bound outside declaration"
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
        | Some fields when isField ctx name ->
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
    | Ast.Call (Identifier name, args) when
        (match ctx.ClassName with Some className -> containsName ctx.FunctionIndex (className + "." + name) | None -> false) ->
        compileExpression ctx (Ast.Call (Member (MeExpr, name), args))
    | Ast.Call (Identifier name, args) ->
        match ctx.ClassFields with
        | Some fields when isField ctx name ->
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
            if args.Length >= 3 then compileExpression ctx (List.item 2 args)
            else emit ctx (LoadConst (addConstant ctx (VString ""))) 0
            emit ctx RaiseErr 0
        | _ -> emit ctx LoadEmpty 0
    | Ast.Call (Member (objExpr, methodName), args) ->
        compileExpression ctx objExpr
        for i in List.length args - 1 .. -1 .. 0 do
            match List.item i args with
            | Identifier name when isField ctx name ->
                emit ctx (LoadLocal 0) 0
                emit ctx (MakeRefMember name) 0
            | Identifier name when not (isField ctx name) ->
                if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                    emit ctx (MakeRefGlobal (getName ctx.Globals name)) 0
                else emit ctx (MakeRefLocal (getLocalSlot ctx name)) 0
            | Ast.Call (Identifier name, indices) when containsSetName ctx.ArrayVars name || containsName ctx.Locals name || containsName ctx.Globals name ->
                compileExpression ctx (Identifier name)
                for index in indices do compileExpression ctx index
                emit ctx (MakeRefArray indices.Length) 0
            | Member (Identifier objectName, _) as arg when objectName.ToLowerInvariant() = "err" ->
                compileExpression ctx arg
            | Member (receiver, memberName) ->
                compileExpression ctx receiver
                emit ctx (MakeRefMember memberName) 0
            | arg -> compileExpression ctx arg
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
        | false, _ -> failwithf "Unknown class %s" className
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
    | LocatedStmt (line, inner) ->
        let start = ctx.Instructions.Count
        emit ctx (Statement -1) line
        compileStatement ctx inner
        patchJump ctx start (Statement ctx.Instructions.Count)
        for i in start .. ctx.Instructions.Count - 1 do
            if ctx.Instructions.[i].LineNumber = 0 then
                ctx.Instructions.[i] <- { ctx.Instructions.[i] with LineNumber = line }
    | ExpressionStmt expr ->
        compileExpression ctx expr
        emit ctx Pop 0

    | Assignment (name, expr) ->
        match ctx.ClassFields with
        | Some fields when isField ctx name ->
            emit ctx (LoadLocal 0) 0
            compileExpression ctx expr
            emit ctx (SetMember (name, false)) 0
        | _ ->
            compileExpression ctx expr
            if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                let slot = getName ctx.Globals name
                emit ctx (StoreGlobal slot) 0
            else
                let slot = getLocalSlot ctx name
                emit ctx (StoreLocal slot) 0

    | IndexedAssignment (name, indices, valueExpr) ->
        compileExpression ctx (Identifier name)
        for index in indices do compileExpression ctx index
        compileExpression ctx valueExpr
        emit ctx (ArraySet indices.Length) 0

    | MemberAssignment (objExpr, memberName, valueExpr, isSet) ->
        compileExpression ctx objExpr
        compileExpression ctx valueExpr
        emit ctx (SetMember (memberName, isSet)) 0

    | Ast.Set (name, expr) ->
        match ctx.ClassFields with
        | Some fields when isField ctx name ->
            emit ctx (LoadLocal 0) 0
            compileExpression ctx expr
            emit ctx (SetMember (name, true)) 0
        | _ ->
            compileExpression ctx expr
            if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
                emit ctx (StoreGlobal (getName ctx.Globals name)) 0
            else
                let slot = getLocalSlot ctx name
                emit ctx (StoreLocal slot) 0

    | Declaration (_, Dim declarators) ->
        for d in declarators do
            if ctx.InFunction then ignore (getLocalSlot ctx d.Name)
            else ignore (getGlobalSlot ctx d.Name)
            match d.ArraySpec with
            | Some spec when not spec.Dimensions.IsEmpty ->
                for dimension in spec.Dimensions do
                    match dimension with
                    | ArrayBound (lo, hi) -> compileExpression ctx lo; compileExpression ctx hi
                    | upper -> emit ctx (LoadConst (addConstant ctx (VInteger 0))) 0; compileExpression ctx upper
                emit ctx (ArrayNew spec.Dimensions.Length) 0
                storeName ctx d.Name
                addSetName ctx.ArrayVars d.Name |> ignore
            | _ -> emit ctx LoadEmpty 0; storeName ctx d.Name

    | Declaration (_, Const (name, _, expr)) ->
        compileExpression ctx expr
        if containsName ctx.Globals name && not (ctx.InFunction && containsName ctx.Locals name) then
            emit ctx (StoreGlobal (getName ctx.Globals name)) 0
        else
            let slot = getLocalSlot ctx name
            emit ctx (StoreLocal slot) 0

    | Declaration (_, ReDim (name, dimensions, preserve)) ->
        if preserve then compileExpression ctx (Identifier name)
        for dimension in dimensions do
            match dimension with
            | ArrayBound (lo, hi) -> compileExpression ctx lo; compileExpression ctx hi
            | upper -> emit ctx (LoadConst (addConstant ctx (VInteger 0))) 0; compileExpression ctx upper
        emit ctx (if preserve then ReDimPreserve dimensions.Length else ArrayNew dimensions.Length) 0
        storeName ctx name
        addSetName ctx.ArrayVars name |> ignore

    | IfStmt (condition, thenStmts, elseIfs, elseStmtsOpt) ->
        compileExpression ctx condition
        let mutable falseJump = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0
        for stmt in thenStmts do compileStatement ctx stmt
        let endJumps = ResizeArray<int>()
        for (elseIfCond, elseIfBody) in elseIfs do
            let jump = ctx.Instructions.Count
            emit ctx (Jump -1) 0
            endJumps.Add jump
            patchJump ctx falseJump (JumpIfFalse ctx.Instructions.Count)
            compileExpression ctx elseIfCond
            falseJump <- ctx.Instructions.Count
            emit ctx (JumpIfFalse -1) 0
            for stmt in elseIfBody do compileStatement ctx stmt
        match elseStmtsOpt with
        | Some body ->
            let jump = ctx.Instructions.Count
            emit ctx (Jump -1) 0
            endJumps.Add jump
            patchJump ctx falseJump (JumpIfFalse ctx.Instructions.Count)
            for stmt in body do compileStatement ctx stmt
        | None -> patchJump ctx falseJump (JumpIfFalse ctx.Instructions.Count)
        for jump in endJumps do patchJump ctx jump (Jump ctx.Instructions.Count)

    | ForLoop (varName, startExpr, endExpr, stepOpt, body) ->
        compileExpression ctx startExpr
        storeName ctx varName
        let endSlot = freshLocal ctx
        compileExpression ctx endExpr
        emit ctx (StoreLocal endSlot) 0
        let stepSlot = freshLocal ctx
        match stepOpt with
        | Some step -> compileExpression ctx step
        | None -> emit ctx (LoadConst (addConstant ctx (VInteger 1))) 0
        emit ctx (StoreLocal stepSlot) 0
        let loopStartPc = ctx.Instructions.Count
        emit ctx (LoadLocal stepSlot) 0
        emit ctx (LoadConst (addConstant ctx (VInteger 0))) 0
        emit ctx LessThan 0
        let positiveJump = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0
        compileExpression ctx (Identifier varName)
        emit ctx (LoadLocal endSlot) 0
        emit ctx LessThan 0
        let joinJump = ctx.Instructions.Count
        emit ctx (Jump -1) 0
        patchJump ctx positiveJump (JumpIfFalse ctx.Instructions.Count)
        compileExpression ctx (Identifier varName)
        emit ctx (LoadLocal endSlot) 0
        emit ctx GreaterThan 0
        patchJump ctx joinJump (Jump ctx.Instructions.Count)
        let exitJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfTrue -1) 0
        ctx.ExitLoopPatches.Add(ResizeArray())
        for stmt in body do compileStatement ctx stmt
        compileExpression ctx (Identifier varName)
        emit ctx (LoadLocal stepSlot) 0
        emit ctx Add 0
        storeName ctx varName
        emit ctx (Jump loopStartPc) 0
        patchJump ctx exitJumpPc (JumpIfTrue ctx.Instructions.Count)
        patchExitLoopTargets ctx

    | ForEach (varName, collExpr, body) ->
        compileExpression ctx collExpr
        let collSlot = freshLocal ctx
        emit ctx (StoreLocal collSlot) 0
        let idxSlot = freshLocal ctx
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
        emit ctx ArrayGetFlat 0
        storeName ctx varName

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
        compileExpression ctx (Ast.Call (Identifier name, args))
        emit ctx Pop 0
    | CallStmt (expr, args) ->
        if args.IsEmpty then compileExpression ctx expr else compileExpression ctx (Ast.Call (expr, args))
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
        | false, _ -> failwithf "Unknown label %s" label

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
    let rec registerLocals stmt =
        match stmt with
        | LocatedStmt (_, inner) -> registerLocals inner
        | Declaration (_, Dim ds) -> for d in ds do ignore (getLocalSlot ctx d.Name)
        | Declaration (_, Const (n, _, _)) -> ignore (getLocalSlot ctx n)
        | IfStmt (_, yes, alternatives, no) ->
            for stmt in yes do registerLocals stmt
            for (_, stmts) in alternatives do
                for stmt in stmts do registerLocals stmt
            match no with
            | Some stmts -> for stmt in stmts do registerLocals stmt
            | None -> ()
        | ForLoop (_, _, _, _, stmts) | ForEach (_, _, stmts) | WhileLoop (_, stmts) | WithStmt (_, stmts) -> for stmt in stmts do registerLocals stmt
        | SelectCase (_, cases) ->
            for (_, stmts) in cases do
                for stmt in stmts do registerLocals stmt
        | DoLoop (a, b) ->
            for condition in [a; b] do
                match condition with
                | Some c -> for stmt in c.Body do registerLocals stmt
                | None -> ()
        | _ -> ()
    for stmt in body do registerLocals stmt

    let funcIndex = getName ctx.FunctionIndex name

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


    let funcDef = {
        Name = name
        ParamsCount = List.length paramList
        LocalsCount = localCount
        ByRefParams = byRefArr
        Code = [||]
        StartPC = startPc
    }
    ctx.Functions.[funcIndex] <- funcDef

let private registerFunction (ctx: CompileContext) name (parameters: Parameter list) =
    if containsName ctx.FunctionIndex name then failwithf "Duplicate procedure %s" name
    let modes = parameters |> List.map (fun p -> p.ByRef) |> List.toArray
    let index = ctx.Functions.Count
    setName ctx.FunctionIndex name index
    setName ctx.ByRefInfo name modes
    ctx.Functions.Add({ Name = name; ParamsCount = modes.Length; LocalsCount = 0
                        ByRefParams = modes; Code = [||]; StartPC = 0 })

let private registerDeclarations (ctx: CompileContext) (tops: TopLevel list) =
    let register prefix func =
        match func with
        | FunctionDecl (_, name, ps, _, _) | SubDecl (_, name, ps, _) -> registerFunction ctx (prefix + name) ps
        | PropertyGet (_, name, ps, _, _) -> registerFunction ctx (prefix + name + "_get") ps
        | PropertyLet (_, name, ps, _) -> registerFunction ctx (prefix + name + "_let") ps
        | PropertySet (_, name, ps, _) -> registerFunction ctx (prefix + name + "_set") ps
    for top in tops do
        match top with
        | FunctionDef f -> register "" f
        | ClassDecl (_, name, members) ->
            if containsName ctx.ClassIndex name then failwithf "Duplicate class %s" name
            setName ctx.ClassIndex name ctx.Classes.Count
            ctx.Classes.Add({ Name = name; Fields = [||]; Methods = Map.empty; PropertyGetters = Map.empty; PropertyLetters = Map.empty; PropertySetters = Map.empty })
            for memberDecl in members do
                match memberDecl with FunctionDef f -> register (name + ".") f | _ -> ()
        | _ -> ()

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
        let outerClassFields = ctx.ClassFields
        let outerClassName = ctx.ClassName
        ctx.ClassName <- Some className
        let mutable fields = []
        let mutable methods = Map.empty
        let mutable getters = Map.empty
        let mutable letters = Map.empty
        let mutable setters = Map.empty

        // First pass: collect field names
        for m in members do
            match m with
            | TopLevelStatement (LocatedStmt (_, Declaration (_, Dim declarators)))
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
                    methods <- Map.add (name.ToLowerInvariant()) (getName ctx.FunctionIndex fullName) methods
                | FunctionDecl (_, name, paramList, _, body) ->
                    let fullName = className + "." + name
                    compileFunctionBody ctx fullName name paramList body
                    methods <- Map.add (name.ToLowerInvariant()) (getName ctx.FunctionIndex fullName) methods
                | PropertyGet (_, name, paramList, _, body) ->
                    let fullName = className + "." + name + "_get"
                    compileFunctionBody ctx fullName name paramList body
                    getters <- Map.add (name.ToLowerInvariant()) (getName ctx.FunctionIndex fullName) getters
                | PropertyLet (_, name, paramList, body) ->
                    let fullName = className + "." + name + "_let"
                    compileFunctionBody ctx fullName name paramList body
                    letters <- Map.add (name.ToLowerInvariant()) (getName ctx.FunctionIndex fullName) letters
                | PropertySet (_, name, paramList, body) ->
                    let fullName = className + "." + name + "_set"
                    compileFunctionBody ctx fullName name paramList body
                    setters <- Map.add (name.ToLowerInvariant()) (getName ctx.FunctionIndex fullName) setters
            | _ -> ()

        let classIdx = getName ctx.ClassIndex className
        ctx.Classes.[classIdx] <- {
            Name = className
            Fields = List.rev fields |> List.toArray
            Methods = methods
            PropertyGetters = getters
            PropertyLetters = letters
            PropertySetters = setters
        }
        ctx.ClassFields <- outerClassFields
        ctx.ClassName <- outerClassName
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
    | TopLevelStatement (LocatedStmt (_, stmt)) -> preRegisterGlobals ctx (TopLevelStatement stmt)
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

    registerDeclarations ctx program.TopLevels
    emit ctx (Jump -1) 0

    for tl in program.TopLevels do
        compileFunctions ctx tl

    patchJump ctx 0 (Jump ctx.Instructions.Count)

    for tl in program.TopLevels do
        compileTopLevel ctx tl

    patchLabels ctx

    emit ctx Halt 0

    {
        TextCompare = program.TopLevels |> List.exists (function OptionStmt option -> option.ToLowerInvariant() = "compare text" | _ -> false)
        Code = ctx.Instructions.ToArray()
        Constants = ctx.Constants.ToArray()
        Globals = ctx.NextGlobalSlot
        GlobalNames = globalNames ctx
        Functions = ctx.Functions.ToArray()
        Classes = ctx.Classes.ToArray()
    }

let compile (program: Program) : BytecodeProgram =
    compileWithGlobalNames Seq.empty program
