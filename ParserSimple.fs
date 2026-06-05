module ParserSimple

open Ast
open Lexer

type ParseResult<'a> =
    | Success of 'a * Token list
    | Error of string * Token

type Parser<'a> = Token list -> ParseResult<'a>

// ── Combinators ──

let (|>>) (parser: Parser<'a>) (f: 'a -> 'b) : Parser<'b> = fun tokens ->
    match parser tokens with
    | Success (result, remaining) -> Success (f result, remaining)
    | Error (msg, token) -> Error (msg, token)

let (>>%) (parser: Parser<'a>) (value: 'b) : Parser<'b> = fun tokens ->
    match parser tokens with
    | Success (_, remaining) -> Success (value, remaining)
    | Error (msg, token) -> Error (msg, token)

let (<|>) (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> = fun tokens ->
    match p1 tokens with
    | Success _ as result -> result
    | Error _ -> p2 tokens

let (.>>.) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a * 'b> = fun tokens ->
    match p1 tokens with
    | Success (result1, remaining1) ->
        match p2 remaining1 with
        | Success (result2, remaining2) -> Success ((result1, result2), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let (.>>) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a> = fun tokens ->
    match (p1 .>>. p2) tokens with
    | Success ((result1, _), remaining) -> Success (result1, remaining)
    | Error (msg, token) -> Error (msg, token)

let (>>.) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'b> = fun tokens ->
    match (p1 .>>. p2) tokens with
    | Success ((_, result2), remaining) -> Success (result2, remaining)
    | Error (msg, token) -> Error (msg, token)

let optional (parser: Parser<'a>) : Parser<'a option> = fun tokens ->
    match parser tokens with
    | Success (result, remaining) -> Success (Some result, remaining)
    | Error _ -> Success (None, tokens)

let many (parser: Parser<'a>) : Parser<'a list> = fun tokens ->
    let rec loop acc remaining =
        match parser remaining with
        | Success (result, newRemaining) -> loop (result :: acc) newRemaining
        | Error _ -> Success (List.rev acc, remaining)
    loop [] tokens

let many1 (parser: Parser<'a>) : Parser<'a list> = fun tokens ->
    match parser tokens with
    | Success (result, remaining) ->
        match many parser remaining with
        | Success (results, finalRemaining) -> Success (result :: results, finalRemaining)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let sepBy (sep: Parser<'b>) (parser: Parser<'a>) : Parser<'a list> = fun tokens ->
    let rec loop acc remaining =
        match parser remaining with
        | Success (result, newRemaining) ->
            match sep newRemaining with
            | Success (_, newerRemaining) -> loop (result :: acc) newerRemaining
            | Error _ -> Success (List.rev (result :: acc), newRemaining)
        | Error _ -> Success (List.rev acc, remaining)
    loop [] tokens

let sepBy1 (sep: Parser<'b>) (parser: Parser<'a>) : Parser<'a list> = fun tokens ->
    match parser tokens with
    | Success (result, remaining) ->
        let rec loop acc rem =
            match sep rem with
            | Success (_, afterSep) ->
                match parser afterSep with
                | Success (r, afterItem) -> loop (r :: acc) afterItem
                | Error _ -> Success (List.rev acc, rem)
            | Error _ -> Success (List.rev acc, rem)
        match loop [result] remaining with
        | Success (results, finalRemaining) -> Success (results, finalRemaining)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let between left right center = (left >>. center .>> right)

// ── Token parsers ──

let ptoken (kind: TokenKind) : Parser<Token> = fun tokens ->
    match tokens with
    | { Kind = k } :: tail when k = kind -> Success (List.head tokens, tail)
    | head :: _ -> Error (sprintf "Expected token of kind %A but got %A" kind head.Kind, head)
    | [] -> Error (sprintf "Expected token of kind %A but reached EOF" kind, { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

let pkeyword (keyword: string) : Parser<Token> = fun tokens ->
    match tokens with
    | { Kind = Keyword; Lexeme = lexeme } :: tail when lexeme.ToLower() = keyword.ToLower() -> Success (List.head tokens, tail)
    | { Kind = Keyword; Lexeme = lexeme } :: _ ->
        Error (sprintf "Expected keyword '%s' but got '%s'" keyword lexeme, List.head tokens)
    | head :: _ -> Error (sprintf "Expected keyword '%s' but got %A" keyword head.Kind, head)
    | [] -> Error (sprintf "Expected keyword '%s' but reached EOF" keyword, { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

let pidentifier : Parser<string> = fun tokens ->
    match tokens with
    | { Kind = Ast.Identifier; Lexeme = lexeme } :: tail -> Success (lexeme, tail)
    | head :: _ -> Error (sprintf "Expected identifier but got %A ('%s')" head.Kind head.Lexeme, head)
    | [] -> Error ("Expected identifier but reached EOF", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

let pnumber : Parser<Expression> = fun tokens ->
    match tokens with
    | { Kind = Number; Lexeme = lexeme } :: tail ->
        let lit =
            if lexeme.Contains "."
            then Literal (Double (float lexeme))
            else Literal (Integer (int lexeme))
        Success (lit, tail)
    | head :: _ -> Error (sprintf "Expected number but got %A" head.Kind, head)
    | [] -> Error ("Expected number but reached EOF", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

let pstring : Parser<Expression> = fun tokens ->
    match tokens with
    | { Kind = StringLiteral; Lexeme = lexeme } :: tail -> Success (Literal (Ast.String lexeme), tail)
    | head :: _ -> Error (sprintf "Expected string but got %A" head.Kind, head)
    | [] -> Error ("Expected string but reached EOF", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

let poperator (op: string) : Parser<unit> = fun tokens ->
    match tokens with
    | { Kind = Operator; Lexeme = lexeme } :: tail when lexeme = op -> Success ((), tail)
    | { Kind = Operator; Lexeme = lexeme } :: _ ->
        Error (sprintf "Expected operator '%s' but got '%s'" op lexeme, List.head tokens)
    | head :: _ -> Error (sprintf "Expected operator '%s' but got %A" op head.Kind, head)
    | [] -> Error (sprintf "Expected operator '%s' but reached EOF" op, { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

/// Skip colon statement separators
let skipColons : Parser<unit> = fun tokens ->
    let rec loop toks =
        match toks with
        | { Kind = Colon } :: tail -> loop tail
        | _ -> toks
    Success ((), loop tokens)

// ── Expression parsers (shared, dialect-independent) ──

/// Accept an identifier or a keyword token as a name (for member access like obj.Error, obj.Type)
let pidentifierOrKeyword : Parser<string> = fun tokens ->
    match tokens with
    | { Kind = Ast.Identifier; Lexeme = lexeme } :: tail -> Success (lexeme, tail)
    | { Kind = Keyword; Lexeme = lexeme } :: tail -> Success (lexeme, tail)
    | head :: _ -> Error (sprintf "Expected identifier or keyword but got %A ('%s')" head.Kind head.Lexeme, head)
    | [] -> Error ("Expected identifier but reached EOF", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

let rec pliteral : Parser<Expression> = fun tokens ->
    let num = pnumber
    let str = pstring
    let true_ = pkeyword "true" >>% Literal (Boolean true)
    let false_ = pkeyword "false" >>% Literal (Boolean false)
    let null_ = pkeyword "null" >>% Literal Null
    let empty_ = pkeyword "empty" >>% Literal Empty
    let nothing_ = pkeyword "nothing" >>% Literal Nothing
    (true_ <|> false_ <|> null_ <|> empty_ <|> nothing_ <|> str <|> num) tokens

and pprimary : Parser<Expression> = fun tokens ->
    let lit = pliteral
    let paren = between (ptoken LParen) (ptoken RParen) pexpression

    let pnew : Parser<Expression> = fun tokens ->
        match (pkeyword "new" .>>. pidentifier) tokens with
        | Success ((_, name), remaining) -> Success (NewExpr name, remaining)
        | Error (msg, token) -> Error (msg, token)

    let pme = pkeyword "me" >>% MeExpr

    let ptypeof : Parser<Expression> = fun tokens ->
        match (pkeyword "typeof" >>. ppostfix .>> pkeyword "is" .>>. pidentifier) tokens with
        | Success ((expr, name), remaining) -> Success (TypeOfIs (expr, name), remaining)
        | Error (msg, token) -> Error (msg, token)

    let ident = pidentifier |>> Identifier

    // Leading dot: .Member inside With block
    let pwithDot : Parser<Expression> = fun tokens ->
        match tokens with
        | { Kind = Dot } :: _ ->
            match (ptoken Dot >>. pidentifierOrKeyword) tokens with
            | Success (name, remaining) -> Success (Member(WithDotExpr, name), remaining)
            | Error (msg, token) -> Error (msg, token)
        | head :: _ -> Error ("Expected dot", head)
        | [] -> Error ("Expected dot", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

    (pnew <|> ptypeof <|> pme <|> lit <|> ident <|> pwithDot <|> paren) tokens

/// Postfix loop: handles .name (member access) and (args) (call/index)
and ppostfix : Parser<Expression> = fun tokens ->
    match pprimary tokens with
    | Success (expr, remaining) ->
        let rec loop acc remaining =
            match remaining with
            | { Kind = Dot; PrecedingNewline = false } :: tail ->
                match pidentifierOrKeyword tail with
                | Success (name, remaining2) -> loop (Member (acc, name)) remaining2
                | Error (msg, token) -> Error (msg, token)
            | { Kind = LParen } :: _ ->
                match (between (ptoken LParen) (ptoken RParen) (sepBy (ptoken Comma) pexpression)) remaining with
                | Success (args, remaining2) -> loop (Call (acc, args)) remaining2
                | Error _ -> Success (acc, remaining)
            | _ -> Success (acc, remaining)
        loop expr remaining
    | Error (msg, token) -> Error (msg, token)

and punary : Parser<Expression> = fun tokens ->
    let not_ = pkeyword "not" >>. punary |>> fun e -> Unary (Not, e)
    let neg = poperator "-" >>. punary |>> fun e -> Unary (Negate, e)
    let pos = poperator "+" >>. punary
    let postfix = ppostfix
    (not_ <|> neg <|> pos <|> postfix) tokens

and pterm : Parser<Expression> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Operator; Lexeme = "*" } :: tail ->
            match punary tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Multiply, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = "/" } :: tail ->
            match punary tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Divide, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = "\\" } :: tail ->
            match punary tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (IntDivide, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Keyword; Lexeme = "mod" } :: tail ->
            match punary tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Modulus, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | _ -> Success (acc, remaining)
    match punary tokens with
    | Success (leftExpr, remaining) -> loop leftExpr remaining
    | Error (msg, token) -> Error (msg, token)

and pfactor : Parser<Expression> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Operator; Lexeme = "^" } :: tail ->
            match pfactor tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Power, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | _ -> Success (acc, remaining)
    match pterm tokens with
    | Success (leftExpr, remaining) -> loop leftExpr remaining
    | Error (msg, token) -> Error (msg, token)

and pconcat : Parser<Expression> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Operator; Lexeme = "&" } :: tail ->
            match pfactor tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Concatenate, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | _ -> Success (acc, remaining)
    match pfactor tokens with
    | Success (leftExpr, remaining) -> loop leftExpr remaining
    | Error (msg, token) -> Error (msg, token)

and paddition : Parser<Expression> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Operator; Lexeme = "+" } :: tail ->
            match pconcat tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Add, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = "-" } :: tail ->
            match pconcat tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Subtract, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | _ -> Success (acc, remaining)
    match pconcat tokens with
    | Success (leftExpr, remaining) -> loop leftExpr remaining
    | Error (msg, token) -> Error (msg, token)

and pcomparison : Parser<Expression> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Eq; Lexeme = "=" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Equal, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = "<>" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (NotEqual, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = "<" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (LessThan, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = "<=" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (LessEqual, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = ">" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (GreaterThan, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Operator; Lexeme = ">=" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (GreaterEqual, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Keyword; Lexeme = "is" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Is, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Keyword; Lexeme = "like" } :: tail ->
            match paddition tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Like, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | _ -> Success (acc, remaining)
    match paddition tokens with
    | Success (leftExpr, remaining) -> loop leftExpr remaining
    | Error (msg, token) -> Error (msg, token)

and pand : Parser<Expression> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Keyword; Lexeme = "and" } :: tail ->
            match pcomparison tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (And, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | _ -> Success (acc, remaining)
    match pcomparison tokens with
    | Success (leftExpr, remaining) -> loop leftExpr remaining
    | Error (msg, token) -> Error (msg, token)

and por : Parser<Expression> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Keyword; Lexeme = "or" } :: tail ->
            match pand tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Or, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Keyword; Lexeme = "xor" } :: tail ->
            match pand tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Xor, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Keyword; Lexeme = "eqv" } :: tail ->
            match pand tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Eqv, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | { Kind = Keyword; Lexeme = "imp" } :: tail ->
            match pand tail with
            | Success (rightExpr, newerRemaining) -> loop (Binary (Imp, acc, rightExpr)) newerRemaining
            | Error (msg, token) -> Error (msg, token)
        | _ -> Success (acc, remaining)
    match pand tokens with
    | Success (leftExpr, remaining) -> loop leftExpr remaining
    | Error (msg, token) -> Error (msg, token)

and pexpression : Parser<Expression> = por

// ── Type reference (VBA) ──

let pTypeRef : Parser<TypeRef> =
    pidentifier |>> fun name -> { Name = name }

// ── Dialect-aware declaration parsing ──

let pVarDeclarator (dialect: Dialect) : Parser<VarDeclarator> = fun tokens ->
    match pidentifier tokens with
    | Success (name, remaining) ->
        // Check for array spec: name(bounds)
        let arraySpec, remaining2 =
            match remaining with
            | { Kind = LParen } :: _ ->
                match (between (ptoken LParen) (ptoken RParen) (sepBy (ptoken Comma) pexpression)) remaining with
                | Success (dims, rem) -> (Some { Dimensions = dims }, rem)
                | Error _ -> (None, remaining)
            | _ -> (None, remaining)
        // Check for As Type (VBA only)
        let typeRef, remaining3 =
            match dialect with
            | VBA ->
                match remaining2 with
                | { Kind = Keyword; Lexeme = "as" } :: _ ->
                    match (pkeyword "as" >>. pTypeRef) remaining2 with
                    | Success (tr, rem) -> (Some tr, rem)
                    | Error _ -> (None, remaining2)
                | _ -> (None, remaining2)
            | VBScript -> (None, remaining2)
        Success ({ Name = name; ArraySpec = arraySpec; Type = typeRef }, remaining3)
    | Error (msg, token) -> Error (msg, token)

let pdim (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match pkeyword "dim" tokens with
    | Success (_, remaining) ->
        match sepBy1 (ptoken Comma) (pVarDeclarator dialect) remaining with
        | Success (declarators, remaining2) ->
            Success (Declaration (None, Dim declarators), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let pconst (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "const" >>. pidentifier) tokens with
    | Success (name, remaining) ->
        // Optional As Type (VBA only)
        let typeRef, remaining2 =
            match dialect with
            | VBA ->
                match remaining with
                | { Kind = Keyword; Lexeme = "as" } :: _ ->
                    match (pkeyword "as" >>. pTypeRef) remaining with
                    | Success (tr, rem) -> (Some tr, rem)
                    | Error _ -> (None, remaining)
                | _ -> (None, remaining)
            | VBScript -> (None, remaining)
        match (ptoken Eq >>. pexpression) remaining2 with
        | Success (expr, remaining3) ->
            Success (Declaration (None, Const (name, typeRef, expr)), remaining3)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let predim : Parser<Statement> = fun tokens ->
    let preserve = optional (pkeyword "preserve") |>> Option.isSome
    match (pkeyword "redim" >>. preserve .>>. pidentifier) tokens with
    | Success ((pres, name), remaining) ->
        match (between (ptoken LParen) (ptoken RParen) pexpression) remaining with
        | Success (sizeExpr, remaining2) ->
            Success (Declaration (None, ReDim (name, sizeExpr, pres)), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

// ── Dialect-aware parameter parsing ──

let pVbsParam : Parser<Parameter> = fun tokens ->
    // [ByVal|ByRef] name [()]
    let passingParser =
        (pkeyword "byref" >>% true) <|> (pkeyword "byval" >>% false)
    let passing = optional passingParser
    match (passing .>>. pidentifier) tokens with
    | Success ((byRefOpt, name), remaining) ->
        let byRef = defaultArg byRefOpt true  // VBS default is ByRef
        // Check for trailing () indicating array param
        let isArray, remaining2 =
            match remaining with
            | { Kind = LParen } :: { Kind = RParen } :: tail -> (true, tail)
            | _ -> (false, remaining)
        Success ({
            Name = name; ByRef = byRef; IsArray = isArray
            Optional = false; IsParamArray = false
            Type = None; DefaultValue = None
        }, remaining2)
    | Error (msg, token) -> Error (msg, token)

let pVbaParam : Parser<Parameter> = fun tokens ->
    // [Optional] [ByVal|ByRef] [ParamArray] name[()] [As Type] [= defaultValue]
    let optionalFlag = optional (pkeyword "optional" >>% true) |>> fun o -> defaultArg o false
    let passingParser =
        (pkeyword "byref" >>% true) <|> (pkeyword "byval" >>% false)
    let passing = optional passingParser
    let paramArrayFlag = optional (pkeyword "paramarray" >>% true) |>> fun o -> defaultArg o false

    match (optionalFlag .>>. passing .>>. paramArrayFlag .>>. pidentifier) tokens with
    | Success ((((isOptional, byRefOpt), isParamArray), name), remaining) ->
        let byRef = defaultArg byRefOpt true
        // trailing ()
        let isArray, remaining2 =
            match remaining with
            | { Kind = LParen } :: { Kind = RParen } :: tail -> (true, tail)
            | _ -> (false, remaining)
        // As Type
        let typeRef, remaining3 =
            match remaining2 with
            | { Kind = Keyword; Lexeme = "as" } :: _ ->
                match (pkeyword "as" >>. pTypeRef) remaining2 with
                | Success (tr, rem) -> (Some tr, rem)
                | Error _ -> (None, remaining2)
            | _ -> (None, remaining2)
        // = defaultValue
        let defaultVal, remaining4 =
            match remaining3 with
            | { Kind = Eq } :: _ ->
                match (ptoken Eq >>. pexpression) remaining3 with
                | Success (expr, rem) -> (Some expr, rem)
                | Error _ -> (None, remaining3)
            | _ -> (None, remaining3)
        Success ({
            Name = name; ByRef = byRef; IsArray = isArray
            Optional = isOptional; IsParamArray = isParamArray
            Type = typeRef; DefaultValue = defaultVal
        }, remaining4)
    | Error (msg, token) -> Error (msg, token)

let pParam (dialect: Dialect) : Parser<Parameter> =
    match dialect with
    | VBA -> pVbaParam
    | VBScript -> pVbsParam

let pParamList (dialect: Dialect) : Parser<Parameter list> = fun tokens ->
    let extractParameters opt =
        match opt with
        | Some (Some ps) -> ps
        | Some None -> []
        | None -> []
    let parametersParser = optional (ptoken LParen >>. optional (sepBy (ptoken Comma) (pParam dialect)) .>> ptoken RParen)
    (parametersParser |>> extractParameters) tokens

// ── Procedure modifiers ──

let parseVisibility : Parser<Visibility> = fun tokens ->
    match tokens with
    | { Kind = Keyword; Lexeme = lexeme } :: tail ->
        match lexeme.ToLower() with
        | "public" -> Success (Public, tail)
        | "private" -> Success (Private, tail)
        | "friend" -> Success (Friend, tail)
        | "global" -> Success (Global, tail)
        | _ -> Error (sprintf "Expected visibility modifier but got '%s'" lexeme, List.head tokens)
    | head :: _ -> Error ("Expected visibility modifier", head)
    | [] -> Error ("Expected visibility modifier but reached EOF", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

let parseProcModifiers (dialect: Dialect) : Parser<Modifier list> = fun tokens ->
    let rec loop acc remaining =
        match remaining with
        | { Kind = Keyword; Lexeme = lexeme } :: tail ->
            match lexeme.ToLower() with
            | "public" -> loop (VisibilityMod Public :: acc) tail
            | "private" -> loop (VisibilityMod Private :: acc) tail
            | "friend" when dialect = VBA -> loop (VisibilityMod Friend :: acc) tail
            | "static" when dialect = VBA -> loop (StaticMod :: acc) tail
            | "default" -> loop (DefaultMod :: acc) tail
            | _ -> Success (List.rev acc, remaining)
        | _ -> Success (List.rev acc, remaining)
    loop [] tokens

let private isModifierAllowed (dialect: Dialect) (kind: ProcKind) (m: Modifier) =
    match dialect with
    | VBScript ->
        match m with
        | VisibilityMod Public | VisibilityMod Private -> true
        | DefaultMod -> kind = PropertyProc
        | _ -> false
    | VBA ->
        match m with
        | VisibilityMod Public | VisibilityMod Private | VisibilityMod Friend -> true
        | StaticMod -> kind = SubProc || kind = FunctionProc
        | DefaultMod -> kind = PropertyProc
        | VisibilityMod Global -> false

let validateProcModifiers (dialect: Dialect) (kind: ProcKind) (mods: Modifier list) (token: Token) : ParseResult<unit> =
    let invalid = mods |> List.tryFind (fun m -> not (isModifierAllowed dialect kind m))
    match invalid with
    | Some m ->
        let modName = match m with
                      | VisibilityMod v -> sprintf "%A" v
                      | StaticMod -> "Static"
                      | DefaultMod -> "Default"
        let kindName = match kind with SubProc -> "Sub" | FunctionProc -> "Function" | PropertyProc -> "Property"
        Error (sprintf "Modifier '%s' is not allowed on %s in %A mode" modName kindName dialect, token)
    | None -> Success ((), [])

// ── Statement parsers ──

/// identifier(args) = expr  →  IndexedAssignment
let rec pindexedAssignment : Parser<Statement> = fun tokens ->
    match tokens with
    | { Kind = Ast.Identifier; Lexeme = name } :: { Kind = LParen } :: _ ->
        match (pidentifier .>>. between (ptoken LParen) (ptoken RParen) (sepBy (ptoken Comma) pexpression)) tokens with
        | Success ((name, indices), remaining) ->
            match remaining with
            | { Kind = Eq } :: _ ->
                match (ptoken Eq >>. pexpression) remaining with
                | Success (value, remaining2) -> Success (IndexedAssignment (name, indices, value), remaining2)
                | Error (msg, token) -> Error (msg, token)
            | _ -> Error ("Expected '=' after indexed target", List.head remaining)
        | Error (msg, token) -> Error (msg, token)
    | head :: _ -> Error ("Expected indexed assignment", head)
    | [] -> Error ("Expected indexed assignment", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

/// expr.member = expr  →  MemberAssignment (tries identifier chains with dots)
and pmemberAssignment : Parser<Statement> = fun tokens ->
    // Parse a postfix expression, then check if it ends with = expr
    match ppostfix tokens with
    | Success (expr, remaining) ->
        match expr with
        | Member (objExpr, memberName) ->
            match remaining with
            | { Kind = Eq } :: _ ->
                match (ptoken Eq >>. pexpression) remaining with
                | Success (value, remaining2) -> Success (MemberAssignment (objExpr, memberName, value), remaining2)
                | Error (msg, token) -> Error (msg, token)
            | _ -> Error ("Expected '=' after member target", List.head remaining)
        | _ -> Error ("Expected member assignment", List.head remaining)
    | Error (msg, token) -> Error (msg, token)

and psetStmtFull : Parser<Statement> = fun tokens ->
    match pkeyword "set" tokens with
    | Success (_, remaining) ->
        // Try member assignment: Set obj.prop = expr
        match ppostfix remaining with
        | Success (expr, remaining2) ->
            match remaining2 with
            | { Kind = Eq } :: _ ->
                match (ptoken Eq >>. pexpression) remaining2 with
                | Success (value, remaining3) ->
                    match expr with
                    | Member (objExpr, memberName) ->
                        Success (MemberAssignment (objExpr, memberName, value), remaining3)
                    | Identifier name ->
                        Success (Set (name, value), remaining3)
                    | Ast.Call (Identifier name, indices) ->
                        Success (IndexedAssignment (name, indices, value), remaining3)
                    | _ -> Error ("Expected identifier or member after Set", List.head remaining)
                | Error (msg, token) -> Error (msg, token)
            | head :: _ -> Error ("Expected '=' after Set target", head)
            | [] -> Error ("Expected '=' after Set target", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pletStmt : Parser<Statement> = fun tokens ->
    let name = pidentifier
    let eq = ptoken Ast.Eq >>. pexpression
    match (name .>>. eq) tokens with
    | Success ((n, e), remaining) -> Success (Assignment (n, e), remaining)
    | Error (msg, token) -> Error (msg, token)

/// Parenthesless Sub/method call: name arg1, arg2  OR  obj.method arg1, arg2
and pSubCallNoParens : Parser<Statement> = fun tokens ->
    // Parse a postfix expression (identifier, obj.method, etc.)
    match ppostfix tokens with
    | Success (target, remaining) ->
        // Only valid for Identifier or Member targets
        match target with
        | Identifier _ | Member _ -> ()
        | _ -> ()
        // Must be followed by an arg on the same line (not =, (, :, newline, EOF)
        match remaining with
        | { Kind = Eq } :: _ -> Error ("Assignment", List.head remaining)
        | { Kind = LParen } :: _ -> Error ("Parenthesized", List.head remaining)
        | { Kind = Colon } :: _ -> Error ("No args", List.head remaining)
        | { Kind = EOF } :: _ -> Error ("No args", List.head remaining)
        | { PrecedingNewline = true } :: _ -> Error ("New line", List.head remaining)
        | [] -> Error ("No args", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
        | _ ->
            match pexpression remaining with
            | Success (firstArg, remaining2) ->
                let rec parseMoreArgs acc rem =
                    match rem with
                    | { Kind = Comma } :: tail ->
                        match pexpression tail with
                        | Success (arg, rem2) -> parseMoreArgs (arg :: acc) rem2
                        | Error _ -> (List.rev acc, rem)
                    | _ -> (List.rev acc, rem)
                let (moreArgs, remaining3) = parseMoreArgs [firstArg] remaining2
                match target with
                | Identifier _ ->
                    Success (CallStmt (target, moreArgs), remaining3)
                | Member (objExpr, methodName) ->
                    // obj.method arg1, arg2 → ExpressionStmt(Call(Member(obj, method), args))
                    Success (ExpressionStmt (Ast.Call (Member (objExpr, methodName), moreArgs)), remaining3)
                | _ ->
                    Success (CallStmt (target, moreArgs), remaining3)
            | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

/// Simple (non-block) statement for single-line If
and pSimpleStmt (dialect: Dialect) : Parser<Statement> = fun tokens ->
    (pcallStmt dialect
     <|> psetStmtFull
     <|> pindexedAssignment
     <|> pmemberAssignment
     <|> pletStmt
     <|> pexit
     <|> pexpressionStmt) tokens

and pifStmt (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "if" >>. pexpression .>> pkeyword "then") tokens with
    | Success (cond, remaining) ->
        // Use PrecedingNewline to distinguish single-line vs block If:
        // If the token after Then is on a new line, it's a block If (needs End If)
        // If on the same line, it's single-line
        match remaining with
        | first :: _ when first.PrecedingNewline ->
            // Block If: token after Then is on a new line
            let thenBlock = many (pstatement dialect)
            let pelseif =
                (pkeyword "elseif" >>. pexpression .>> pkeyword "then") .>>. many (pstatement dialect)
            let elseBlock = optional (pkeyword "else" >>. many (pstatement dialect))
            let end_ = skipColons >>. pkeyword "end" >>. pkeyword "if"
            match (thenBlock .>>. many pelseif .>>. elseBlock .>>. end_) remaining with
            | Success ((((thenStmts, elseIfs), elseStmtsOpt), _), remaining2) ->
                Success (IfStmt (cond, thenStmts, elseIfs, elseStmtsOpt), remaining2)
            | Error (msg, token) -> Error (msg, token)
        | _ ->
            // Single-line If: token after Then is on the same line
            // Also handles colon-separated: If x Then : y = 1 : End If
            match remaining with
            | { Kind = Colon } :: _ ->
                // Colon after Then → treat as block If (the : acts like a newline)
                let thenBlock = many (pstatement dialect)
                let pelseif =
                    (pkeyword "elseif" >>. pexpression .>> pkeyword "then") .>>. many (pstatement dialect)
                let elseBlock = optional (pkeyword "else" >>. many (pstatement dialect))
                let end_ = skipColons >>. pkeyword "end" >>. pkeyword "if"
                match (thenBlock .>>. many pelseif .>>. elseBlock .>>. end_) remaining with
                | Success ((((thenStmts, elseIfs), elseStmtsOpt), _), remaining2) ->
                    Success (IfStmt (cond, thenStmts, elseIfs, elseStmtsOpt), remaining2)
                | Error (msg, token) -> Error (msg, token)
            | _ ->
                // True single-line: If cond Then stmt [Else stmt]
                match pSimpleStmt dialect remaining with
                | Success (thenStmt, remaining2) ->
                    match remaining2 with
                    | { Kind = Keyword; Lexeme = "else" } :: _ ->
                        match (pkeyword "else" >>. pSimpleStmt dialect) remaining2 with
                        | Success (elseStmt, remaining3) ->
                            Success (IfStmt (cond, [thenStmt], [], Some [elseStmt]), remaining3)
                        | Error _ ->
                            Success (IfStmt (cond, [thenStmt], [], None), remaining2)
                    | _ ->
                        Success (IfStmt (cond, [thenStmt], [], None), remaining2)
                | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pforLoop (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let var = (pkeyword "for" >>. pidentifier)
    let start = (ptoken Eq >>. pexpression)
    let end_ = (pkeyword "to" >>. pexpression)
    let step = optional (pkeyword "step" >>. pexpression)
    let body = many (pstatement dialect)
    let next = (skipColons >>. pkeyword "next")
    let headerParser = var .>>. start .>>. end_ .>>. step
    match headerParser tokens with
    | Success ((((v, s), e), stepOpt), remainingAfterHeader) ->
        match body remainingAfterHeader with
        | Success (body, remainingAfterBody) ->
            match next remainingAfterBody with
            | Success (_, remainingFinal) ->
                // Optionally consume trailing loop variable after Next (e.g. "Next i")
                let remainingFinal =
                    match remainingFinal with
                    | { Kind = Ast.Identifier; Lexeme = name } :: tail when name.ToLower() = v.ToLower() -> tail
                    | _ -> remainingFinal
                Success (ForLoop (v, s, e, stepOpt, body), remainingFinal)
            | Error (msg, token) -> Error (msg, token)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pforEach (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "for" >>. pkeyword "each" >>. pidentifier .>> pkeyword "in" .>>. pexpression) tokens with
    | Success ((varName, collExpr), remaining) ->
        match (many (pstatement dialect) .>> skipColons .>> pkeyword "next") remaining with
        | Success (body, remaining2) -> Success (ForEach (varName, collExpr, body), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pwhileLoop (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let cond = pkeyword "while" >>. pexpression
    let body = many (pstatement dialect)
    let end_ = skipColons >>. pkeyword "wend"
    match ((cond .>>. body) .>> end_) tokens with
    | Success ((c, body), remaining) ->
        Success (WhileLoop (c, body), remaining)
    | Error (msg, token) -> Error (msg, token)

and pdoLoop (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let body = many (pstatement dialect)

    let parseDoWhileCond tokens =
        match (pkeyword "do" .>>. ((pkeyword "while" <|> pkeyword "until") .>>. pexpression) .>>. body .>> skipColons .>>. pkeyword "loop") tokens with
        | Success ((((_, (condToken, cond)), body), _), remaining) ->
            let isWhile = condToken.Lexeme.ToLower() = "while"
            if isWhile then
                Success (DoLoop (Some { Condition = cond; Body = body }, None), remaining)
            else
                let negatedCond = Unary (Not, cond)
                Success (DoLoop (Some { Condition = negatedCond; Body = body }, None), remaining)
        | Error (msg, token) -> Error (msg, token)

    let parseLoopUntilCond tokens =
        match (pkeyword "do" .>>. body .>> skipColons .>>. pkeyword "loop" .>>. optional ((pkeyword "while" <|> pkeyword "until") .>>. pexpression)) tokens with
        | Success ((((_, body), _), condOpt), remaining) ->
            match condOpt with
            | Some (condToken, cond) ->
                let isWhile = condToken.Lexeme.ToLower() = "while"
                if isWhile then
                    Success (DoLoop (None, Some { Condition = cond; Body = body }), remaining)
                else
                    let negatedCond = Unary (Not, cond)
                    Success (DoLoop (None, Some { Condition = negatedCond; Body = body }), remaining)
            | None ->
                Success (DoLoop (None, Some { Condition = Literal (Boolean false); Body = body }), remaining)
        | Error (msg, token) -> Error (msg, token)

    parseDoWhileCond <|> parseLoopUntilCond <| tokens

and pexit : Parser<Statement> = fun tokens ->
    let exitKeyword = pkeyword "exit"
    match exitKeyword tokens with
    | Success (_, remaining) ->
        match remaining with
        | { Kind = Keyword; Lexeme = lexeme } :: tail ->
            let stmt =
                match lexeme.ToLower() with
                | "for" -> ExitFor
                | "do" -> ExitDo
                | "sub" -> ExitSub
                | "function" -> ExitFunction
                | "property" -> ExitProperty
                | _ -> ExitFor
            Success (stmt, tail)
        | _ -> Error ("Expected keyword after 'exit' (for, do, sub, function, property)", List.head remaining)
    | Error (msg, token) -> Error (msg, token)

and pcaseTest : Parser<CaseTest> = fun tokens ->
    // Try "Is <op> expr"
    match tokens with
    | { Kind = Keyword; Lexeme = "is" } :: { Kind = Operator; Lexeme = op } :: _ ->
        let binOp = match op with
                    | "<" -> LessThan | "<=" -> LessEqual
                    | ">" -> GreaterThan | ">=" -> GreaterEqual
                    | "<>" -> NotEqual | _ -> Equal
        match (pkeyword "is" >>. ptoken Operator >>. pexpression) tokens with
        | Success (expr, rem) -> Success (CaseComparison(binOp, expr), rem)
        | Error (m, t) -> Error (m, t)
    | { Kind = Keyword; Lexeme = "is" } :: { Kind = Eq } :: _ ->
        match (pkeyword "is" >>. ptoken Eq >>. pexpression) tokens with
        | Success (expr, rem) -> Success (CaseComparison(Equal, expr), rem)
        | Error (m, t) -> Error (m, t)
    | _ ->
        match pexpression tokens with
        | Success (expr1, remaining) ->
            // Check for "To expr" (range)
            match remaining with
            | { Kind = Keyword; Lexeme = "to" } :: _ ->
                match (pkeyword "to" >>. pexpression) remaining with
                | Success (expr2, rem2) -> Success (CaseRange(expr1, expr2), rem2)
                | Error (m, t) -> Error (m, t)
            | _ -> Success (CaseValue expr1, remaining)
        | Error (m, t) -> Error (m, t)

and pcase (dialect: Dialect) : Parser<CaseTest list option * Statement list> = fun tokens ->
    let caseKeyword = skipColons >>. pkeyword "case"
    let elseKeyword = skipColons >>. pkeyword "case" >>. pkeyword "else"

    let caseValues = sepBy1 (ptoken Comma) pcaseTest |>> fun tests -> Some tests
    let normalCase = caseKeyword >>. caseValues
    let elseCase = elseKeyword |>> fun _ -> None

    match (normalCase <|> elseCase) tokens with
    | Success (cond, remaining) ->
        match many (pstatement dialect) remaining with
        | Success (stmts, remaining2) -> Success ((cond, stmts), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pselectCase (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let selectExpr = pkeyword "select" >>. pkeyword "case" >>. pexpression
    let cases = many1 (pcase dialect)
    let endSelect = skipColons >>. pkeyword "end" >>. pkeyword "select"

    match (selectExpr .>>. cases .>>. endSelect) tokens with
    | Success (((expr, cases), _), remaining) ->
        Success (SelectCase (expr, cases), remaining)
    | Error (msg, token) -> Error (msg, token)

and pOnError (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "on" >>. pkeyword "error") tokens with
    | Success (_, remaining) ->
        match remaining with
        | { Kind = Keyword; Lexeme = "resume" } :: { Kind = Keyword; Lexeme = "next" } :: tail ->
            Success (OnError ResumeNext, tail)
        | { Kind = Keyword; Lexeme = "goto" } :: tail ->
            match tail with
            | { Kind = Number; Lexeme = "0" } :: tail2 ->
                Success (OnError GoToZero, tail2)
            | _ when dialect = VBA ->
                match pidentifier tail with
                | Success (label, remaining2) -> Success (OnError (GoToLabel label), remaining2)
                | Error (msg, token) -> Error (msg, token)
            | head :: _ -> Error ("Expected '0' after 'On Error GoTo' in VBScript", head)
            | [] -> Error ("Expected target after 'On Error GoTo'", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
        | head :: _ -> Error ("Expected 'Resume Next' or 'GoTo' after 'On Error'", head)
        | [] -> Error ("Unexpected EOF after 'On Error'", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
    | Error (msg, token) -> Error (msg, token)

and pwithStmt (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "with" >>. pexpression) tokens with
    | Success (expr, remaining) ->
        match (many (pstatement dialect) .>> skipColons .>> pkeyword "end" .>> pkeyword "with") remaining with
        | Success (body, remaining2) -> Success (WithStmt (expr, body), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and peraseStmt : Parser<Statement> = fun tokens ->
    match (pkeyword "erase" >>. pidentifier) tokens with
    | Success (name, remaining) -> Success (EraseStmt name, remaining)
    | Error (msg, token) -> Error (msg, token)

and pcallStmt (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "call" >>. ppostfix) tokens with
    | Success (expr, remaining) -> Success (CallStmt (expr, []), remaining)
    | Error (msg, token) -> Error (msg, token)

and pgotoStmt : Parser<Statement> = fun tokens ->
    match (pkeyword "goto" >>. pidentifier) tokens with
    | Success (label, remaining) -> Success (GoToStmt label, remaining)
    | Error (msg, token) -> Error (msg, token)

and pgosubStmt : Parser<Statement> = fun tokens ->
    match (pkeyword "gosub" >>. pidentifier) tokens with
    | Success (label, remaining) -> Success (GoSubStmt label, remaining)
    | Error (msg, token) -> Error (msg, token)

and preturnStmt : Parser<Statement> = fun tokens ->
    match pkeyword "return" tokens with
    | Success (_, remaining) -> Success (ReturnStmt, remaining)
    | Error (msg, token) -> Error (msg, token)

and plabel : Parser<Statement> = fun tokens ->
    match tokens with
    | { Kind = Ast.Identifier; Lexeme = name } :: { Kind = Colon } :: tail ->
        Success (LabelStmt name, tail)
    | head :: _ -> Error ("Expected label", head)
    | [] -> Error ("Expected label but reached EOF", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

and pVisibilityDecl (dialect: Dialect) : Parser<Statement> = fun tokens ->
    // Public/Private/Global Dim-like declarations (without Dim keyword)
    match parseVisibility tokens with
    | Success (vis, remaining) ->
        // Check if it's a Const
        match remaining with
        | { Kind = Keyword; Lexeme = "const" } :: _ ->
            match (pkeyword "const" >>. pidentifier) remaining with
            | Success (name, remaining2) ->
                // Optional As Type (VBA only)
                let typeRef, remaining3 =
                    match dialect with
                    | VBA ->
                        match remaining2 with
                        | { Kind = Keyword; Lexeme = "as" } :: _ ->
                            match (pkeyword "as" >>. pTypeRef) remaining2 with
                            | Success (tr, rem) -> (Some tr, rem)
                            | Error _ -> (None, remaining2)
                        | _ -> (None, remaining2)
                    | VBScript -> (None, remaining2)
                match (ptoken Eq >>. pexpression) remaining3 with
                | Success (expr, remaining4) ->
                    Success (Declaration (Some vis, Const (name, typeRef, expr)), remaining4)
                | Error (msg, token) -> Error (msg, token)
            | Error (msg, token) -> Error (msg, token)
        | _ ->
            // Variable declaration: Public x, y As Integer  (or just Public x in VBS)
            match sepBy1 (ptoken Comma) (pVarDeclarator dialect) remaining with
            | Success (declarators, remaining2) ->
                Success (Declaration (Some vis, Dim declarators), remaining2)
            | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pexpressionStmt : Parser<Statement> = fun tokens ->
    match pexpression tokens with
    | Success (expr, remaining) -> Success (ExpressionStmt expr, remaining)
    | Error (msg, token) -> Error (msg, token)

and pstatement (dialect: Dialect) : Parser<Statement> = fun tokens ->
    // Try label before skipColons (VBA only), since label ends with ':'
    if dialect = VBA then
        match plabel tokens with
        | Success _ as result -> result
        | Error _ ->
        pstatementAfterColons dialect tokens
    else
        pstatementAfterColons dialect tokens

and pstatementAfterColons (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match skipColons tokens with
    | Success ((), tokens) ->
        // Try label after colons too (VBA only)
        match (if dialect = VBA then plabel tokens else Error ("", List.head tokens)) with
        | Success _ as result -> result
        | Error _ ->
        let parsers =
            pselectCase dialect
            <|> pOnError dialect
            <|> pwithStmt dialect
            <|> peraseStmt
            <|> predim
            <|> pdim dialect
            <|> pconst dialect
            <|> psetStmtFull
            <|> pifStmt dialect
            <|> pforEach dialect
            <|> pforLoop dialect
            <|> pwhileLoop dialect
            <|> pdoLoop dialect
            <|> pexit
            <|> pcallStmt dialect
            <|> pgotoStmt
            <|> pgosubStmt
            <|> preturnStmt
            <|> pVisibilityDecl dialect
            <|> pindexedAssignment
            <|> pmemberAssignment
            <|> pletStmt
            <|> pSubCallNoParens
            <|> pexpressionStmt
        parsers tokens
    | Error (msg, token) -> Error (msg, token)

// ── Function/Sub/Property parsing ──

let pfunction (dialect: Dialect) : Parser<Function> = fun tokens ->
    match parseProcModifiers dialect tokens with
    | Success (mods, remaining) ->
        match remaining with
        | { Kind = Keyword; Lexeme = lexeme } :: _ when lexeme.ToLower() = "function" ->
            match validateProcModifiers dialect FunctionProc mods (List.head remaining) with
            | Error (msg, token) -> Error (msg, token)
            | _ ->
            match (pkeyword "function" >>. pidentifier .>>. pParamList dialect) remaining with
            | Success ((name, parameters), remaining2) ->
                // Return type (VBA only)
                let returnType, remaining3 =
                    match dialect with
                    | VBA ->
                        match remaining2 with
                        | { Kind = Keyword; Lexeme = "as" } :: _ ->
                            match (pkeyword "as" >>. pTypeRef) remaining2 with
                            | Success (tr, rem) -> (Some tr, rem)
                            | Error _ -> (None, remaining2)
                        | _ -> (None, remaining2)
                    | VBScript -> (None, remaining2)
                match (many (pstatement dialect) .>> skipColons .>> pkeyword "end" .>> pkeyword "function") remaining3 with
                | Success (body, remaining4) ->
                    Success (FunctionDecl (mods, name, parameters, returnType, body), remaining4)
                | Error (msg, token) -> Error (msg, token)
            | Error (msg, token) -> Error (msg, token)

        | { Kind = Keyword; Lexeme = lexeme } :: _ when lexeme.ToLower() = "sub" ->
            match validateProcModifiers dialect SubProc mods (List.head remaining) with
            | Error (msg, token) -> Error (msg, token)
            | _ ->
            match (pkeyword "sub" >>. pidentifier .>>. pParamList dialect) remaining with
            | Success ((name, parameters), remaining2) ->
                match (many (pstatement dialect) .>> skipColons .>> pkeyword "end" .>> pkeyword "sub") remaining2 with
                | Success (body, remaining3) ->
                    Success (SubDecl (mods, name, parameters, body), remaining3)
                | Error (msg, token) -> Error (msg, token)
            | Error (msg, token) -> Error (msg, token)

        | { Kind = Keyword; Lexeme = lexeme } :: _ when lexeme.ToLower() = "property" ->
            match validateProcModifiers dialect PropertyProc mods (List.head remaining) with
            | Error (msg, token) -> Error (msg, token)
            | _ ->
            match (pkeyword "property" >>. (pkeyword "get" <|> pkeyword "let" <|> pkeyword "set")) remaining with
            | Success (kindToken, remaining2) ->
                match (pidentifier .>>. pParamList dialect) remaining2 with
                | Success ((name, parameters), remaining3) ->
                    let kind = kindToken.Lexeme.ToLower()
                    match kind with
                    | "get" ->
                        let returnType, remaining4 =
                            match dialect with
                            | VBA ->
                                match remaining3 with
                                | { Kind = Keyword; Lexeme = "as" } :: _ ->
                                    match (pkeyword "as" >>. pTypeRef) remaining3 with
                                    | Success (tr, rem) -> (Some tr, rem)
                                    | Error _ -> (None, remaining3)
                                | _ -> (None, remaining3)
                            | VBScript -> (None, remaining3)
                        match (many (pstatement dialect) .>> skipColons .>> pkeyword "end" .>> pkeyword "property") remaining4 with
                        | Success (body, remaining5) ->
                            Success (PropertyGet (mods, name, parameters, returnType, body), remaining5)
                        | Error (msg, token) -> Error (msg, token)
                    | "let" ->
                        match (many (pstatement dialect) .>> skipColons .>> pkeyword "end" .>> pkeyword "property") remaining3 with
                        | Success (body, remaining4) ->
                            Success (PropertyLet (mods, name, parameters, body), remaining4)
                        | Error (msg, token) -> Error (msg, token)
                    | "set" ->
                        match (many (pstatement dialect) .>> skipColons .>> pkeyword "end" .>> pkeyword "property") remaining3 with
                        | Success (body, remaining4) ->
                            Success (PropertySet (mods, name, parameters, body), remaining4)
                        | Error (msg, token) -> Error (msg, token)
                    | _ -> Error (sprintf "Expected Get, Let, or Set after Property but got '%s'" kind, kindToken)
                | Error (msg, token) -> Error (msg, token)
            | Error (msg, token) -> Error (msg, token)

        | head :: _ -> Error (sprintf "Expected Function, Sub, or Property but got '%s'" head.Lexeme, head)
        | [] -> Error ("Expected procedure declaration", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
    | Error (msg, token) -> Error (msg, token)

// ── VBA-only declaration parsers ──

let peventDecl (dialect: Dialect) : Parser<TopLevel> = fun tokens ->
    let vis = optional parseVisibility
    match (vis .>>. pkeyword "event" .>>. pidentifier .>>. pParamList dialect) tokens with
    | Success ((((visOpt, _), name), parms), remaining) ->
        Success (EventDecl (visOpt, name, parms), remaining)
    | Error (msg, token) -> Error (msg, token)

let pWithEventsDecl (dialect: Dialect) : Parser<TopLevel> = fun tokens ->
    let vis = optional parseVisibility
    match (vis .>>. pkeyword "withevents" .>>. pidentifier .>> pkeyword "as" .>>. pTypeRef) tokens with
    | Success ((((visOpt, _), name), typeRef), remaining) ->
        Success (WithEventsDecl (visOpt, name, typeRef), remaining)
    | Error (msg, token) -> Error (msg, token)

let pdeclare (dialect: Dialect) : Parser<TopLevel> = fun tokens ->
    // [Public|Private] Declare [Sub|Function] name Lib "libname" [Alias "aliasname"] ([params]) [As type]
    let vis = optional parseVisibility
    match (vis .>>. pkeyword "declare") tokens with
    | Success ((visOpt, _), remaining) ->
        let isFunction =
            match remaining with
            | { Kind = Keyword; Lexeme = lexeme } :: _ when lexeme.ToLower() = "function" -> true
            | _ -> false
        match ((pkeyword "function" <|> pkeyword "sub") >>. pidentifier .>> pkeyword "lib" .>>. pstring) remaining with
        | Success ((name, libExpr), remaining2) ->
            let libName = match libExpr with Literal (String s) -> s | _ -> ""
            // Optional Alias
            let aliasName, remaining3 =
                match remaining2 with
                | { Kind = Keyword; Lexeme = "alias" } :: _ ->
                    match (pkeyword "alias" >>. pstring) remaining2 with
                    | Success (aliasExpr, rem) ->
                        let alias = match aliasExpr with Literal (String s) -> Some s | _ -> None
                        (alias, rem)
                    | Error _ -> (None, remaining2)
                | _ -> (None, remaining2)
            // Parameter list
            match pParamList dialect remaining3 with
            | Success (parms, remaining4) ->
                // Return type (Function only)
                let returnType, remaining5 =
                    if isFunction then
                        match remaining4 with
                        | { Kind = Keyword; Lexeme = "as" } :: _ ->
                            match (pkeyword "as" >>. pTypeRef) remaining4 with
                            | Success (tr, rem) -> (Some tr, rem)
                            | Error _ -> (None, remaining4)
                        | _ -> (None, remaining4)
                    else (None, remaining4)
                let info = {
                    IsFunction = isFunction
                    Name = name
                    LibName = libName
                    AliasName = aliasName
                    Parameters = parms
                    ReturnType = returnType
                }
                Success (DeclareDecl (visOpt, info), remaining5)
            | Error (msg, token) -> Error (msg, token)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

// ── VBA-only top-level forms ──

let penum : Parser<TopLevel> = fun tokens ->
    let vis = optional parseVisibility
    match (vis .>>. pkeyword "enum" .>>. pidentifier) tokens with
    | Success (((visOpt, _), name), remaining) ->
        let rec parseMembers acc remaining =
            match skipColons remaining with
            | Success ((), remaining) ->
                match (pkeyword "end" >>. pkeyword "enum") remaining with
                | Success (_, remaining2) -> Success (List.rev acc, remaining2)
                | Error _ ->
                    match pidentifier remaining with
                    | Success (memberName, remaining2) ->
                        let value, remaining3 =
                            match remaining2 with
                            | { Kind = Eq } :: _ ->
                                match (ptoken Eq >>. pexpression) remaining2 with
                                | Success (expr, rem) -> (Some expr, rem)
                                | Error _ -> (None, remaining2)
                            | _ -> (None, remaining2)
                        parseMembers ({ Name = memberName; Value = value } :: acc) remaining3
                    | Error (msg, token) -> Error (msg, token)
            | Error (msg, token) -> Error (msg, token)
        match parseMembers [] remaining with
        | Success (members, remaining2) -> Success (EnumDecl (visOpt, name, members), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let ptypeDecl : Parser<TopLevel> = fun tokens ->
    let vis = optional parseVisibility
    match (vis .>>. pkeyword "type" .>>. pidentifier) tokens with
    | Success (((visOpt, _), name), remaining) ->
        let rec parseMembers acc remaining =
            match skipColons remaining with
            | Success ((), remaining) ->
                match (pkeyword "end" >>. pkeyword "type") remaining with
                | Success (_, remaining2) -> Success (List.rev acc, remaining2)
                | Error _ ->
                    match (pidentifier .>> pkeyword "as" .>>. pTypeRef) remaining with
                    | Success ((memberName, typeRef), remaining2) ->
                        parseMembers ({ Name = memberName; Type = typeRef; ArraySpec = None } :: acc) remaining2
                    | Error (msg, token) -> Error (msg, token)
            | Error (msg, token) -> Error (msg, token)
        match parseMembers [] remaining with
        | Success (members, remaining2) -> Success (TypeDecl (visOpt, name, members), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let pimplements : Parser<TopLevel> = fun tokens ->
    match (pkeyword "implements" >>. pidentifier) tokens with
    | Success (name, remaining) -> Success (ImplementsDecl name, remaining)
    | Error (msg, token) -> Error (msg, token)

let poption : Parser<TopLevel> = fun tokens ->
    match pkeyword "option" tokens with
    | Success (_, remaining) ->
        // Option name can be a keyword (e.g. "Explicit") or identifier
        match remaining with
        | { Kind = Keyword; Lexeme = lexeme } :: tail -> Success (OptionStmt lexeme, tail)
        | { Kind = Ast.Identifier; Lexeme = lexeme } :: tail -> Success (OptionStmt lexeme, tail)
        | head :: _ -> Error ("Expected option name after 'Option'", head)
        | [] -> Error ("Expected option name after 'Option'", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
    | Error (msg, token) -> Error (msg, token)

// ── Class parsing ──

let pclass (dialect: Dialect) : Parser<TopLevel> = fun tokens ->
    let vis = optional parseVisibility
    match (vis .>>. pkeyword "class" .>>. pidentifier) tokens with
    | Success (((visOpt, _), name), remaining) ->
        let rec parseMembers acc remaining =
            match skipColons remaining with
            | Success ((), remaining) ->
                // Check for End Class first
                match (pkeyword "end" >>. pkeyword "class") remaining with
                | Success (_, remaining2) -> Success (List.rev acc, remaining2)
                | Error _ ->
                // VBA-only class members: Event, WithEvents, Enum
                let tryVbaClassMember =
                    match dialect with
                    | VBA ->
                        match (peventDecl dialect) remaining with
                        | Success (tl, rem) -> Some (tl, rem)
                        | Error _ ->
                        match (pWithEventsDecl dialect) remaining with
                        | Success (tl, rem) -> Some (tl, rem)
                        | Error _ ->
                        match penum remaining with
                        | Success (tl, rem) -> Some (tl, rem)
                        | Error _ -> None
                    | VBScript -> None
                match tryVbaClassMember with
                | Some (tl, remaining2) -> parseMembers (tl :: acc) remaining2
                | None ->
                // Try function/sub/property
                match pfunction dialect remaining with
                | Success (func, remaining2) -> parseMembers (FunctionDef func :: acc) remaining2
                | Error _ ->
                // Try a statement (Dim, Public x, Private x, etc.)
                match pstatement dialect remaining with
                | Success (stmt, remaining2) -> parseMembers (TopLevelStatement stmt :: acc) remaining2
                | Error (msg, token) -> Error (msg, token)
            | Error (msg, token) -> Error (msg, token)
        match parseMembers [] remaining with
        | Success (members, remaining2) -> Success (ClassDecl (visOpt, name, members), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

// ── Top-level parsing ──

let pTopLevel (dialect: Dialect) : Parser<TopLevel> = fun tokens ->
    match skipColons tokens with
    | Success ((), tokens) ->
        let tryClass = pclass dialect

        let tryFunc =
            pfunction dialect |>> FunctionDef

        // VBA-only top-level forms
        let tryVbaOnly =
            match dialect with
            | VBA ->
                penum <|> ptypeDecl <|> pimplements
                <|> pdeclare dialect
                <|> peventDecl dialect <|> pWithEventsDecl dialect
            | VBScript ->
                fun _ -> Error ("", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

        let tryOption = poption

        let tryStmt =
            pstatement dialect |>> TopLevelStatement

        (tryClass <|> tryFunc <|> tryVbaOnly <|> tryOption <|> tryStmt) tokens
    | Error (msg, token) -> Error (msg, token)

let pprogram (dialect: Dialect) : Parser<Program> = fun tokens ->
    match many (pTopLevel dialect) tokens with
    | Success (topLevels, remaining) ->
        match skipColons remaining with
        | Success ((), remaining) ->
            Success ({ Dialect = dialect; TopLevels = topLevels }, remaining)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

/// Strict mode: parser enforces dialect grammar rules at parse time
let parse_dialect (dialect: Dialect) (source: string) : Result<Program, string> =
    let tokens = Lexer.tokenize source
    match pprogram dialect tokens with
    | Success (program, _) -> Result.Ok program
    | Error (msg, token) -> Result.Error (sprintf "Parse error at line %d, column %d: %s" token.Line token.Column msg)

/// Tolerant mode: parse as VBA superset, tag with target dialect for validation
/// Use this for migration tools, source converters, or "what's incompatible?" analysis
let parse_tolerant (dialect: Dialect) (source: string) : Result<Program, string> =
    let tokens = Lexer.tokenize source
    match pprogram VBA tokens with
    | Success (program, _) -> Result.Ok { program with Dialect = dialect }
    | Error (msg, token) -> Result.Error (sprintf "Parse error at line %d, column %d: %s" token.Line token.Column msg)

let parse (source: string) : Result<Program, string> =
    parse_dialect VBScript source
