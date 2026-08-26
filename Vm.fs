module Rbt.Vb.Vm

open Ast

type Value =
    | VInteger of int
    | VDouble of float
    | VString of string
    | VBoolean of bool
    | VNull
    | VEmpty
    | VArray of Value array
    | VObject of Map<string, Value>
    | VNothing

type Environment = Map<string, Value>

type VmState = {
    Env: Environment
    ReturnValue: Value option
    ExitRequested: bool
    ExitType: string option
}

let emptyState = {
    Env = Map.empty
    ReturnValue = None
    ExitRequested = false
    ExitType = None
}

let rec valueToInt = function
    | VInteger i -> i
    | VDouble d -> int d
    | VString s -> 
        match System.Int32.TryParse(s) with
        | true, i -> i
        | _ -> 0
    | VBoolean b -> if b then 1 else 0
    | _ -> 0

let rec valueToDouble = function
    | VInteger i -> float i
    | VDouble d -> d
    | VString s ->
        match System.Double.TryParse(s) with
        | true, d -> d
        | _ -> 0.0
    | VBoolean b -> if b then 1.0 else 0.0
    | _ -> 0.0

let rec valueToString = function
    | VInteger i -> string i
    | VDouble d -> string d
    | VString s -> s
    | VBoolean b -> if b then "True" else "False"
    | VNull -> "Null"
    | VEmpty -> "Empty"
    | VNothing -> "Nothing"
    | VArray arr -> "[" + System.String.Join(", ", arr |> Array.map valueToString) + "]"
    | VObject obj -> "{" + System.String.Join(", ", obj |> Map.toList |> List.map (fun (k, v) -> sprintf "%s: %s" k (valueToString v))) + "}"

let rec valueToBool = function
    | VBoolean b -> b
    | VInteger i -> i <> 0
    | VDouble d -> d <> 0.0
    | VString s -> s.Length > 0
    | VNull | VEmpty | VNothing -> false
    | VArray arr -> arr.Length > 0
    | VObject obj -> obj.Count > 0

let rec evalExpression (state: VmState) (expr: Expression) =
    match expr with
    | Literal lit -> evalLiteral lit, state
    | Identifier name ->
        match Map.tryFind name state.Env with
        | Some value -> value, state
        | None -> VEmpty, state
    | Unary (op, expr) ->
        let (value, state) = evalExpression state expr
        let result = 
            match op with
            | Not -> VBoolean (not (valueToBool value))
            | Negate -> 
                match value with
                | VInteger i -> VInteger (-i)
                | VDouble d -> VDouble (-d)
                | _ -> VInteger 0
        result, state
    | Binary (op, left, right) ->
        let (lval, state) = evalExpression state left
        let (rval, state) = evalExpression state right
        let result = 
            match op with
            | Add -> 
                match lval, rval with
                | VInteger i1, VInteger i2 -> VInteger (i1 + i2)
                | VDouble d1, VDouble d2 -> VDouble (d1 + d2)
                | VString s1, _ -> VString (s1 + valueToString rval)
                | _, VString s2 -> VString (valueToString lval + s2)
                | _ -> VDouble (valueToDouble lval + valueToDouble rval)
            | Subtract ->
                match lval, rval with
                | VInteger i1, VInteger i2 -> VInteger (i1 - i2)
                | VDouble d1, VDouble d2 -> VDouble (d1 - d2)
                | _ -> VDouble (valueToDouble lval - valueToDouble rval)
            | Multiply ->
                match lval, rval with
                | VInteger i1, VInteger i2 -> VInteger (i1 * i2)
                | VDouble d1, VDouble d2 -> VDouble (d1 * d2)
                | _ -> VDouble (valueToDouble lval * valueToDouble rval)
            | Divide ->
                let d2 = valueToDouble rval
                if d2 = 0.0 then VDouble 0.0
                elif valueToDouble lval % valueToDouble rval = 0.0
                then VInteger (int (valueToDouble lval / d2))
                else VDouble (valueToDouble lval / d2)
            | Modulus ->
                let i1 = valueToInt lval
                let i2 = valueToInt rval
                if i2 = 0 then VInteger 0
                else VInteger (i1 % i2)
            | Power ->
                let d1 = valueToDouble lval
                let d2 = valueToDouble rval
                VDouble (System.Math.Pow(d1, d2))
            | Concatenate -> VString (valueToString lval + valueToString rval)
            | Equal -> VBoolean (valueToString lval = valueToString rval)
            | NotEqual -> VBoolean (valueToString lval <> valueToString rval)
            | LessThan ->
                match lval, rval with
                | VString s1, VString s2 -> VBoolean (s1 < s2)
                | _ -> VBoolean (valueToDouble lval < valueToDouble rval)
            | LessEqual ->
                match lval, rval with
                | VString s1, VString s2 -> VBoolean (s1 <= s2)
                | _ -> VBoolean (valueToDouble lval <= valueToDouble rval)
            | GreaterThan ->
                match lval, rval with
                | VString s1, VString s2 -> VBoolean (s1 > s2)
                | _ -> VBoolean (valueToDouble lval > valueToDouble rval)
            | GreaterEqual ->
                match lval, rval with
                | VString s1, VString s2 -> VBoolean (s1 >= s2)
                | _ -> VBoolean (valueToDouble lval >= valueToDouble rval)
            | And -> VBoolean (valueToBool lval && valueToBool rval)
            | Or -> VBoolean (valueToBool lval || valueToBool rval)
            | Xor -> VBoolean ((valueToBool lval) <> (valueToBool rval))
            | Eqv -> VBoolean ((valueToBool lval) = (valueToBool rval))
            | Imp -> VBoolean ((not (valueToBool lval)) || (valueToBool rval))
            | Is -> VBoolean (System.Object.ReferenceEquals(lval, rval))
        result, state
    | Call (name, args) ->
        let (argValues, state) = 
            List.fold (fun (acc, state) arg ->
                let (value, state) = evalExpression state arg
                (value :: acc, state)
            ) ([], state) args
        let argValues = List.rev argValues
        let result = 
            match name.ToLower() with
            | "len" | "length" ->
                match argValues with
                | [VString s] -> VInteger s.Length
                | [VArray arr] -> VInteger arr.Length
                | _ -> VInteger 0
            | "left" ->
                match argValues with
                | [VString s; VInteger n] -> VString (if n > 0 then s.[..min (n-1) (s.Length-1)] else "")
                | _ -> VString ""
            | "right" ->
                match argValues with
                | [VString s; VInteger n] -> VString (if n > 0 then s.[max 0 (s.Length-n)..] else "")
                | _ -> VString ""
            | "mid" ->
                match argValues with
                | [VString s; VInteger start] -> VString (if start > 0 then s.[start-1..] else "")
                | [VString s; VInteger start; VInteger len] -> 
                    if start > 0 && len > 0
                    then VString (s.[start-1..min (start+len-2) (s.Length-1)])
                    else VString ""
                | _ -> VString ""
            | "ucase" | "toupper" ->
                match argValues with
                | [VString s] -> VString (s.ToUpper())
                | _ -> VString ""
            | "lcase" | "tolower" ->
                match argValues with
                | [VString s] -> VString (s.ToLower())
                | _ -> VString ""
            | "trim" ->
                match argValues with
                | [VString s] -> VString (s.Trim())
                | _ -> VString ""
            | "cint" ->
                match argValues with
                | [v] -> VInteger (valueToInt v)
                | _ -> VInteger 0
            | "cdbl" ->
                match argValues with
                | [v] -> VDouble (valueToDouble v)
                | _ -> VDouble 0.0
            | "cstr" ->
                match argValues with
                | [v] -> VString (valueToString v)
                | _ -> VString ""
            | "cbool" ->
                match argValues with
                | [v] -> VBoolean (valueToBool v)
                | _ -> VBoolean false
            | "abs" ->
                match argValues with
                | [VInteger i] -> VInteger (abs i)
                | [VDouble d] -> VDouble (abs d)
                | _ -> VInteger 0
            | "sqr" ->
                match argValues with
                | [VInteger i] -> VDouble (sqrt (float i))
                | [VDouble d] -> VDouble (sqrt d)
                | _ -> VDouble 0.0
            | "int" ->
                match argValues with
                | [VInteger i] -> VInteger i
                | [VDouble d] -> VInteger (int (System.Math.Floor d))
                | _ -> VInteger 0
            | "msgbox" ->
                match argValues with
                | [VString msg] -> 
                    printfn "%s" msg
                    VInteger 1
                | _ -> VInteger 0
            | "print" ->
                match argValues with
                | [v] -> 
                    printfn "%s" (valueToString v)
                    VEmpty
                | _ -> VEmpty
            | _ -> VEmpty
        result, state
    | Member (obj, prop) ->
        let (objVal, state) = evalExpression state obj
        match objVal with
        | VObject objMap ->
            match Map.tryFind prop objMap with
            | Some value -> value, state
            | None -> VEmpty, state
        | _ -> VEmpty, state
    | ArrayAccess (name, index) ->
        let (idxVal, state) = evalExpression state index
        let idx = valueToInt idxVal
        match Map.tryFind name state.Env with
        | Some (VArray arr) when idx >= 0 && idx < arr.Length -> arr.[idx], state
        | _ -> VEmpty, state

and evalLiteral = function
    | Integer i -> VInteger i
    | Double d -> VDouble d
    | String s -> VString s
    | Boolean b -> VBoolean b
    | Null -> VNull
    | Empty -> VEmpty

let rec evalStatement (state: VmState) (stmt: Statement) =
    eprintfn "Executing: %A" stmt
    if state.ExitRequested then state
    else
        match stmt with
        | ExpressionStmt expr ->
            let (_, state) = evalExpression state expr
            state
        | Let (name, expr) ->
            let (value, state) = evalExpression state expr
            { state with Env = Map.add name value state.Env }
        | Set (name, expr) ->
            let (value, state) = evalExpression state expr
            { state with Env = Map.add name value state.Env }
        | Assignment (name, expr) ->
            let (value, state) = evalExpression state expr
            { state with Env = Map.add name value state.Env }
        | Declaration (Dim (name, Some expr)) ->
            let (value, state) = evalExpression state expr
            { state with Env = Map.add name value state.Env }
        | Declaration (Dim (name, None)) ->
            { state with Env = Map.add name VEmpty state.Env }
        | Declaration (ReDim _) -> state
        | IfStmt (cond, thenStmts, elseStmts) ->
            let (condVal, state) = evalExpression state cond
            if valueToBool condVal then
                List.fold evalStatement state thenStmts
            else
                match elseStmts with
                | Some elseStmts -> List.fold evalStatement state elseStmts
                | None -> state
        | SelectCase (expr, cases) ->
            let (testValue, state) = evalExpression state expr
            let rec matchCase cases =
                match cases with
                | [] -> state
                | (None, stmts) :: rest ->
                    List.fold evalStatement state stmts
                | (Some caseExpr, stmts) :: rest ->
                    let (caseValue, state) = evalExpression state caseExpr
                    if valueToString testValue = valueToString caseValue then
                        List.fold evalStatement state stmts
                    else
                        matchCase rest
            matchCase cases
        | ForLoop (var, start, end_, step, body) ->
            let (startVal, state) = evalExpression state start
            let (endVal, state) = evalExpression state end_
            let (stepVal, state) = 
                match step with
                | Some stepExpr -> evalExpression state stepExpr
                | None -> VInteger 1, state
            let step = valueToInt stepVal
            if step = 0 then state
            else
                let rec loop state current =
                    if (step > 0 && valueToInt current > valueToInt endVal) || (step < 0 && valueToInt current < valueToInt endVal)
                    then state
                    else
                        let state = { state with Env = Map.add var current state.Env }
                        let state = List.fold evalStatement state body
                        if state.ExitRequested then state
                        else
                            let (currentVal, state) = evalExpression { state with Env = Map.remove var state.Env } (Binary (Add, Literal (Integer (valueToInt current)), Literal (Integer step)))
                            loop state currentVal
                loop state startVal
        | ForEach (var, expr, body) ->
            let (collection, state) = evalExpression state expr
            match collection with
            | VArray arr ->
                let rec loop state idx =
                    if idx >= arr.Length then state
                    else
                        let state = { state with Env = Map.add var arr.[idx] state.Env }
                        let state = List.fold evalStatement state body
                        if state.ExitRequested then state
                        else loop state (idx + 1)
                loop state 0
            | VString s ->
                let rec loop state idx =
                    if idx >= s.Length then state
                    else
                        let state = { state with Env = Map.add var (VString (string s.[idx])) state.Env }
                        let state = List.fold evalStatement state body
                        if state.ExitRequested then state
                        else loop state (idx + 1)
                loop state 0
            | _ -> state
        | WhileLoop (cond, body) ->
            let rec loop state =
                let (condVal, state) = evalExpression state cond
                if not (valueToBool condVal) || state.ExitRequested
                then state
                else
                    let state = List.fold evalStatement state body
                    loop state
            loop state
        | DoLoop (whilePart, untilPart) ->
            match whilePart, untilPart with
            | Some (cond, body), None ->
                let rec loop state =
                    let (condVal, state) = evalExpression state cond
                    if not (valueToBool condVal) || state.ExitRequested
                    then state
                    else
                        let state = List.fold evalStatement state body
                        loop state
                loop state
            | None, Some (body, cond) ->
                let rec loop state =
                    let state = List.fold evalStatement state body
                    if state.ExitRequested then state
                    else
                        let (condVal, state) = evalExpression state cond
                        if valueToBool condVal then state
                        else loop state
                loop state
            | _ -> state
        | ExitFor | ExitDo | ExitSub | ExitFunction ->
            let exitType = 
                match stmt with
                | ExitFor -> Some "for"
                | ExitDo -> Some "do"
                | ExitSub -> Some "sub"
                | ExitFunction -> Some "function"
                | _ -> None
            { state with ExitRequested = true; ExitType = exitType }
        | CallStmt (name, args) ->
            let (argValues, state) = 
                List.fold (fun (acc, state) arg ->
                    let (value, state) = evalExpression state arg
                    (value :: acc, state)
                ) ([], state) args
            let argValues = List.rev argValues
            match name.ToLower() with
            | "print" ->
                eprintfn "Print called with args: %A" argValues
                match argValues with
                | [v] -> printfn "%s" (valueToString v)
                | _ -> printfn "%s" (System.String.Join(" ", argValues |> List.map valueToString))
            | "msgbox" ->
                match argValues with
                | [VString msg] -> printfn "%s" msg
                | _ -> ()
            | _ -> ()
            state

let run (program: Program) =
    let initialState = { emptyState with Env = Map.empty }
    let state = List.fold evalStatement initialState program.Statements
    match state.ReturnValue with
    | Some value -> value
    | None -> VEmpty
