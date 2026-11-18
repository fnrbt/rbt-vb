module Rbt.Vb.ParserSimple

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

let private numberLiteral (text: string) =
    if text.StartsWith "&" then
        let isLong = text.EndsWith "&" && text.Length > 2
        let body = text.Substring(2).TrimEnd('&', '%')
        let radix = if text.[1] = 'h' || text.[1] = 'H' then 16 else 8
        let mutable value = 0.0
        for c in body do
            let digit = if c >= '0' && c <= '9' then int c - int '0' else int (System.Char.ToUpperInvariant c) - int 'A' + 10
            value <- value * float radix + float digit
        if value > 4294967295.0 then failwithf "Overflow in literal %s" text
        // Without a & suffix, values that fit 16 bits are Integers (&HFFFF = -1).
        if not isLong && value <= 65535.0 then
            Literal (Integer (int (if value >= 32768.0 then value - 65536.0 else value)))
        else Literal (Long (int (if value >= 2147483648.0 then value - 4294967296.0 else value)))
    else
        let suffix = text.[text.Length - 1]
        let hasSuffix = not (System.Char.IsDigit suffix || suffix = '.')
        let body = if hasSuffix then text.Substring(0, text.Length - 1) else text
        let isWhole = body |> Seq.forall System.Char.IsDigit
#if FABLE_COMPILER
        let number = float body
#else
        let number = System.Double.Parse(body, System.Globalization.CultureInfo.InvariantCulture)
#endif
        match (if hasSuffix then suffix else ' ') with
        | '%' ->
            if not isWhole || number > 32767.0 then failwithf "Overflow in literal %s" text
            Literal (Integer (int number))
        | '&' ->
            if not isWhole || number > 2147483647.0 then failwithf "Overflow in literal %s" text
            Literal (Long (int number))
        | '!' -> Literal (Single (NumberText.toSingle number))
        | '#' -> Literal (Double number)
        | '@' ->
            let digits = NumberText.ofText body
            let units = NumberText.integerPart { digits with Exponent = digits.Exponent + 4 }
            if units.Length > 19 then failwithf "Overflow in literal %s" text
            Literal (Currency (System.Int64.Parse units))
        | _ when isWhole && number <= 32767.0 -> Literal (Integer (int number))
        | _ when isWhole && number <= 2147483647.0 -> Literal (Long (int number))
        | _ -> Literal (Double number)

let pnumber : Parser<Expression> = fun tokens ->
    match tokens with
    | { Kind = Number; Lexeme = lexeme } :: tail ->
        Success (numberLiteral lexeme, tail)
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

let private topLevelHeadAfterModifiers tokens =
    let rec loop remaining =
        match remaining with
        | { Kind = Keyword; Lexeme = lexeme } :: tail ->
            match lexeme.ToLower() with
            | "public" | "private" | "friend" | "global" | "static" | "default" -> loop tail
            | _ -> remaining
        | _ -> remaining
    loop tokens

// ── Expression parsers (shared, dialect-independent) ──

/// Accept an identifier or a keyword token as a name (for member access like obj.Error, obj.Type)
let pidentifierOrKeyword : Parser<string> = fun tokens ->
    match tokens with
    | { Kind = Ast.Identifier; Lexeme = lexeme } :: tail -> Success (lexeme, tail)
    | { Kind = Keyword; Lexeme = lexeme } :: tail -> Success (lexeme, tail)
    | head :: _ -> Error (sprintf "Expected identifier or keyword but got %A ('%s')" head.Kind head.Lexeme, head)
    | [] -> Error ("Expected identifier but reached EOF", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

/// A type or class name, possibly qualified: `Long`, `Scripting.Dictionary`.
let pqualifiedName : Parser<string> = fun tokens ->
    match pidentifierOrKeyword tokens with
    | Success (first, rest) ->
        let rec loop (name: string) toks =
            match toks with
            | { Kind = Dot; PrecedingNewline = false } :: { Kind = (Ast.Identifier | Keyword); Lexeme = part; PrecedingNewline = false } :: tail ->
                loop (name + "." + part) tail
            | _ -> Success (name, toks)
        loop first rest
    | Error (msg, token) -> Error (msg, token)

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
    match tokens with
    | { Kind = Keyword; Lexeme = lexeme } :: tail ->
        match lexeme.ToLower() with
        | "new" ->
            match pqualifiedName tail with
            | Success (name, remaining) -> Success (NewExpr name, remaining)
            | Error (msg, token) -> Error (msg, token)
        | "typeof" ->
            match (ppostfix .>> pkeyword "is" .>>. pidentifier) tail with
            | Success ((expr, name), remaining) -> Success (TypeOfIs (expr, name), remaining)
            | Error (msg, token) -> Error (msg, token)
        | "me" -> Success (MeExpr, tail)
        | "true" -> Success (Literal (Boolean true), tail)
        | "false" -> Success (Literal (Boolean false), tail)
        | "null" -> Success (Literal Null, tail)
        | "empty" -> Success (Literal Empty, tail)
        | "nothing" -> Success (Literal Nothing, tail)
        | _ -> Error (sprintf "Expected expression but got keyword '%s'" lexeme, List.head tokens)
    | { Kind = Number; Lexeme = lexeme } :: tail ->
        Success (numberLiteral lexeme, tail)
    | { Kind = StringLiteral; Lexeme = lexeme } :: tail ->
        Success (Literal (Ast.String lexeme), tail)
    | { Kind = BracketText; Lexeme = text } :: tail ->
        Success (Call (Identifier "[evaluate]", [ Literal (Ast.String text) ]), tail)
    | { Kind = DateLiteral; Lexeme = lexeme } :: tail ->
        match DateCore.tryParse lexeme System.DateTime.Now.Year with
        | Some serial -> Success (Literal (Ast.Date serial), tail)
        | None -> Error ("Invalid date literal", List.head tokens)
    | { Kind = Ast.Identifier; Lexeme = lexeme } :: tail ->
        Success (Identifier lexeme, tail)
    | { Kind = Dot } :: tail ->
        match pidentifierOrKeyword tail with
        | Success (name, remaining) -> Success (Member(WithDotExpr, name), remaining)
        | Error (msg, token) -> Error (msg, token)
    | { Kind = LParen } :: _ ->
        (between (ptoken LParen) (ptoken RParen) pexpression |>> Parenthesized) tokens
    | head :: _ -> Error (sprintf "Expected expression but got %A ('%s')" head.Kind head.Lexeme, head)
    | [] -> Error ("Expected expression but reached EOF", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

/// One call argument: `expr` or `name:=expr`.
and pargument : Parser<Expression> = fun tokens ->
    match tokens with
    | { Kind = (Ast.Identifier | Keyword); Lexeme = name } :: { Kind = Colon } :: { Kind = Eq; PrecedingNewline = false } :: rest ->
        match pexpression rest with
        | Success (value, next) -> Success (NamedArg (name, value), next)
        | Error (msg, token) -> Error (msg, token)
    | _ -> pexpression tokens

/// `(a, , c)`: an argument list in which arguments may be omitted.
and pargList : Parser<Expression list> = fun tokens ->
    match tokens with
    | { Kind = LParen } :: { Kind = RParen } :: rest -> Success ([], rest)
    | { Kind = LParen } :: rest ->
        let rec loop acc toks =
            let item, afterItem =
                match toks with
                | { Kind = Comma } :: _ | { Kind = RParen } :: _ -> Success (MissingArg, toks), toks
                | _ -> pargument toks, toks
            match item with
            | Error (msg, token) -> Error (msg, token)
            | Success (arg, next) ->
                ignore afterItem
                match next with
                | { Kind = Comma } :: tail -> loop (arg :: acc) tail
                | { Kind = RParen } :: tail -> Success (List.rev (arg :: acc), tail)
                | head :: _ -> Error ("Expected ',' or ')' in argument list", head)
                | [] -> Error ("Expected ')' but reached EOF", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
        loop [] rest
    | head :: _ -> Error ("Expected '('", head)
    | [] -> Error ("Expected '(' but reached EOF", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

/// Postfix loop: handles .name (member access) and (args) (call/index)
and ppostfix : Parser<Expression> = fun tokens ->
    match pprimary tokens with
    | Success (expr, remaining) ->
        let rec loop acc remaining =
            match remaining with
            | { Kind = Dot; Lexeme = "."; PrecedingNewline = false } :: tail ->
                match pidentifierOrKeyword tail with
                | Success (name, remaining2) -> loop (Member (acc, name)) remaining2
                | Error (msg, token) -> Error (msg, token)
            | { Kind = LParen; PrecedingNewline = false } :: _ ->
                match pargList remaining with
                | Success (args, remaining2) -> loop (Call (acc, args)) remaining2
                | Error (msg, token) -> Error (msg, token)
            | _ -> Success (acc, remaining)
        loop expr remaining
    | Error (msg, token) -> Error (msg, token)

// VB evaluates repeated binary operators at the same precedence from left to right,
// including exponentiation. Exponentiation binds tighter than unary minus.
and ppower : Parser<Expression> = fun tokens ->
    match ppostfix tokens with
    | Error (msg, token) -> Error (msg, token)
    | Success (first, rest) ->
        let rec loop left rem =
            match rem with
            | { Kind = Operator; Lexeme = "^"; PrecedingNewline = false } :: tail ->
                let rec signedAtom toks =
                    match toks with
                    | { Kind = Operator; Lexeme = "-" } :: rest ->
                        match signedAtom rest with
                        | Success (expr, next) -> Success (Unary (Negate, expr), next)
                        | Error (msg, token) -> Error (msg, token)
                    | { Kind = Operator; Lexeme = "+" } :: rest -> signedAtom rest
                    | _ -> ppostfix toks
                match signedAtom tail with
                | Success (right, next) -> loop (Binary (Power, left, right)) next
                | Error (msg, token) -> Error (msg, token)
            | _ -> Success (left, rem)
        loop first rest

and punary : Parser<Expression> = fun tokens ->
    match tokens with
    | { Kind = Operator; Lexeme = "-"; PrecedingNewline = false } :: tail ->
        match punary tail with
        | Success (expr, rest) -> Success (Unary (Negate, expr), rest)
        | Error (msg, token) -> Error (msg, token)
    | { Kind = Operator; Lexeme = "+"; PrecedingNewline = false } :: tail -> punary tail
    | _ -> ppower tokens

and pchain (operand: Parser<Expression>) (operators: (Token -> BinaryOperator option)) : Parser<Expression> = fun tokens ->
    match operand tokens with
    | Error (msg, token) -> Error (msg, token)
    | Success (first, rest) ->
        let rec loop left rem =
            match rem with
            | token :: tail when not token.PrecedingNewline ->
                match operators token with
                | Some op ->
                    match operand tail with
                    | Success (right, next) -> loop (Binary (op, left, right)) next
                    | Error (msg, bad) -> Error (msg, bad)
                | None -> Success (left, rem)
            | _ -> Success (left, rem)
        loop first rest

and pmultiply : Parser<Expression> =
    pchain punary (fun t -> match t.Kind, t.Lexeme with Operator, "*" -> Some Multiply | Operator, "/" -> Some Divide | _ -> None)
and pintdivide : Parser<Expression> =
    pchain pmultiply (fun t -> match t.Kind, t.Lexeme with Operator, "\\" -> Some IntDivide | _ -> None)
and pmodulus : Parser<Expression> =
    pchain pintdivide (fun t -> match t.Kind, t.Lexeme with Keyword, "mod" -> Some Modulus | _ -> None)
and paddition : Parser<Expression> =
    pchain pmodulus (fun t -> match t.Kind, t.Lexeme with Operator, "+" -> Some Add | Operator, "-" -> Some Subtract | _ -> None)
and pconcat : Parser<Expression> =
    pchain paddition (fun t -> match t.Kind, t.Lexeme with Operator, "&" -> Some Concatenate | _ -> None)
and pcomparison : Parser<Expression> =
    pchain pconcat (fun t ->
        match t.Kind, t.Lexeme with
        | Eq, _ -> Some Equal
        | Operator, "<>" -> Some NotEqual
        | Operator, "<" -> Some LessThan
        | Operator, "<=" -> Some LessEqual
        | Operator, ">" -> Some GreaterThan
        | Operator, ">=" -> Some GreaterEqual
        | Keyword, "is" -> Some Is
        | Keyword, "like" -> Some Like
        | _ -> None)
and pnot : Parser<Expression> = fun tokens ->
    match tokens with
    | { Kind = Keyword; Lexeme = "not" } :: tail ->
        match pnot tail with
        | Success (expr, rest) -> Success (Unary (Not, expr), rest)
        | Error (msg, token) -> Error (msg, token)
    | _ -> pcomparison tokens
and pand : Parser<Expression> =
    pchain pnot (fun t -> if t.Kind = Keyword && t.Lexeme = "and" then Some And else None)
and por : Parser<Expression> =
    pchain pand (fun t -> if t.Kind = Keyword && t.Lexeme = "or" then Some Or else None)
and pxor : Parser<Expression> =
    pchain por (fun t -> if t.Kind = Keyword && t.Lexeme = "xor" then Some Xor else None)
and peqqv : Parser<Expression> =
    pchain pxor (fun t -> if t.Kind = Keyword && t.Lexeme = "eqv" then Some Eqv else None)
and pimp : Parser<Expression> =
    pchain peqqv (fun t -> if t.Kind = Keyword && t.Lexeme = "imp" then Some Imp else None)
and pexpression : Parser<Expression> = pimp

// ── Type reference (VBA) ──

let pTypeRef : Parser<TypeRef> =
    pqualifiedName |>> fun name -> { Name = name }

let pDimension : Parser<Expression> = fun tokens ->
    match pexpression tokens with
    | Success (lower, { Kind = Keyword; Lexeme = "to" } :: rest) ->
        match pexpression rest with
        | Success (upper, remaining) -> Success (ArrayBound (lower, upper), remaining)
        | Error (msg, token) -> Error (msg, token)
    | result -> result

let pDimensions allowEmpty : Parser<Expression list> = fun tokens ->
    match tokens with
    | { Kind = LParen } :: { Kind = RParen } :: rest when allowEmpty -> Success ([], rest)
    | _ -> (between (ptoken LParen) (ptoken RParen) (sepBy1 (ptoken Comma) pDimension)) tokens

// ── Dialect-aware declaration parsing ──

let pVarDeclarator (dialect: Dialect) : Parser<VarDeclarator> = fun tokens ->
    match pidentifier tokens with
    | Error (msg, token) -> Error (msg, token)
    | Success (_, _) when (List.head tokens).PrecedingNewline ->
        Error ("Expected variable name on the same logical line", List.head tokens)
    | Success (name, remaining) ->
        let dimensions =
            match remaining with
            | { Kind = LParen } :: _ ->
                match pDimensions true remaining with
                | Success (dims, rest) -> Success (Some { Dimensions = dims }, rest)
                | Error (msg, token) -> Error (msg, token)
            | _ -> Success (None, remaining)
        match dimensions with
        | Error (msg, token) -> Error (msg, token)
        | Success (arraySpec, rest) ->
            match dialect, rest with
            | VBA, { Kind = Keyword; Lexeme = "as" } :: { Kind = Keyword; Lexeme = "new" } :: tail ->
                match pTypeRef tail with
                | Success (typeRef, next) ->
                    Success ({ Name = name; ArraySpec = arraySpec; Type = Some typeRef; IsNew = true }, next)
                | Error (msg, token) -> Error (msg, token)
            | VBA, { Kind = Keyword; Lexeme = "as" } :: tail ->
                match pTypeRef tail with
                | Success (typeRef, next) ->
                    // `As String * 10`: the fixed length is not kept.
                    let next =
                        match next with
                        | { Kind = Operator; Lexeme = "*"; PrecedingNewline = false } :: { Kind = (Number | Ast.Identifier); PrecedingNewline = false } :: rest -> rest
                        | _ -> next
                    Success ({ Name = name; ArraySpec = arraySpec; Type = Some typeRef; IsNew = false }, next)
                | Error (msg, token) -> Error (msg, token)
            | _ -> Success ({ Name = name; ArraySpec = arraySpec; Type = None; IsNew = false }, rest)

let pdimWith (keyword: string) (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match pkeyword keyword tokens with
    | Success (_, remaining) ->
        match remaining with
        | { PrecedingNewline = true } :: _ -> Error ("Expected variable name after Dim", List.head remaining)
        | _ ->
            match sepBy1 (ptoken Comma) (pVarDeclarator dialect) remaining with
            | Success (declarators, remaining2) ->
                Success (Declaration (None, (if keyword = "static" then StaticDim declarators else Dim declarators)), remaining2)
            | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let pdim dialect = pdimWith "dim" dialect

/// `name [As Type] = value`, one item of a Const statement.
let private pconstItem (dialect: Dialect) : Parser<string * TypeRef option * Expression> = fun tokens ->
    match pidentifier tokens with
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
        | Success (expr, remaining3) -> Success ((name, typeRef, expr), remaining3)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

/// `Const A = 1, B As Long = 2` after the Const keyword.
let pconstItems (dialect: Dialect) (visibility: Visibility option) : Parser<Statement> = fun tokens ->
    match sepBy1 (ptoken Comma) (pconstItem dialect) tokens with
    | Success ([ (name, typeRef, expr) ], rest) -> Success (Declaration (visibility, Const (name, typeRef, expr)), rest)
    | Success (items, rest) ->
        Success (StatementGroup [ for (name, typeRef, expr) in items -> Declaration (visibility, Const (name, typeRef, expr)) ], rest)
    | Error (msg, token) -> Error (msg, token)

let pconst (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match pkeyword "const" tokens with
    | Success (_, remaining) -> pconstItems dialect None remaining
    | Error (msg, token) -> Error (msg, token)

let predim : Parser<Statement> = fun tokens ->
    let preserve = optional (pkeyword "preserve") |>> Option.isSome
    match (pkeyword "redim" >>. preserve .>>. pidentifier) tokens with
    | Success ((pres, name), remaining) ->
        match pDimensions false remaining with
        | Success (dimensions, remaining2) ->
            // `ReDim a(n) As T`: the element type comes from the Dim; the clause is accepted.
            let remaining3 =
                match remaining2 with
                | { Kind = Keyword; Lexeme = "as"; PrecedingNewline = false } :: { Kind = Ast.Identifier } :: rest -> rest
                | _ -> remaining2
            Success (Declaration (None, ReDim (name, dimensions, pres)), remaining3)
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
    match tokens with
    | { Kind = LParen } :: { Kind = RParen } :: rest -> Success ([], rest)
    | { Kind = LParen } :: _ ->
        (between (ptoken LParen) (ptoken RParen) (sepBy1 (ptoken Comma) (pParam dialect))) tokens
    | _ -> Success ([], tokens)

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

/// `End` on its own (not `End If`, `End Sub`, ...) stops the program.
let isEndStatement (tokens: Token list) =
    match tokens with
    | { Kind = Keyword; Lexeme = "end" } :: next :: _ ->
        next.Kind = EOF || next.Kind = Colon || next.PrecedingNewline ||
        (next.Kind = Keyword && next.Lexeme = "else")
    | [ { Kind = Keyword; Lexeme = "end" } ] -> true
    | _ -> false

let rec psetStmtFull : Parser<Statement> = fun tokens ->
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
                        Success (MemberAssignment (objExpr, memberName, value, true), remaining3)
                    | Identifier name ->
                        Success (Set (name, value), remaining3)
                    | Ast.Call (Identifier name, indices) ->
                        Success (IndexedAssignment (name, indices, value, true), remaining3)
                    | Ast.Call _ ->
                        Success (DefaultAssignment (expr, value, true), remaining3)
                    | _ -> Error ("Expected identifier or member after Set", List.head remaining)
                | Error (msg, token) -> Error (msg, token)
            | head :: _ -> Error ("Expected '=' after Set target", head)
            | [] -> Error ("Expected '=' after Set target", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

/// Parenthesless Sub/method call: name arg1, arg2  OR  obj.method arg1, arg2
and pSubCallNoParens : Parser<Statement> = fun tokens ->
    // `Name (a) & b` / `obj.Method (1), 2`: the parentheses belong to the first
    // argument when more of the statement follows them. The callee is the
    // dotted name before the parenthesis.
    let rec dotted (toks: Token list) count =
        match toks with
        | { Kind = Ast.Identifier } :: { Kind = Dot } :: rest -> dotted rest (count + 2)
        | { Kind = Ast.Identifier } :: { Kind = LParen } :: _ -> Some (count + 1)
        | _ -> None
    let rec closing (toks: Token list) depth =
        match toks with
        | { Kind = LParen } :: rest -> closing rest (depth + 1)
        | { Kind = RParen } :: rest -> if depth = 1 then Some rest else closing rest (depth - 1)
        | { Kind = EOF } :: _ | [] -> None
        | { PrecedingNewline = true } :: _ when depth = 0 -> None
        | _ :: rest -> closing rest depth
    let splitTarget =
        match dotted tokens 0 with
        | Some count when (match tokens with { Kind = Ast.Identifier; Lexeme = l } :: _ -> l.ToLowerInvariant() <> "call" | _ -> false) ->
            let afterName = List.skip count tokens
            match closing afterName 0 with
            | Some (next :: _) when not next.PrecedingNewline
                                    && (next.Kind = Operator || next.Kind = Comma
                                        || (next.Kind = Keyword && List.contains (next.Lexeme.ToLowerInvariant()) [ "and"; "or"; "xor"; "mod"; "like"; "eqv"; "imp" ])) ->
                match ppostfix (List.take count tokens @ [ { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false } ]) with
                | Success (target, _) -> Some (target, afterName)
                | Error _ -> None
            | _ -> None
        | _ -> None
    let postfix =
        match splitTarget with
        | Some (target, rest) -> Success (target, rest)
        | None -> ppostfix tokens
    match postfix with
    | Success (target, remaining) ->
        // Must be followed by an arg on the same line (not =, (, :, newline, EOF)
        match remaining with
        | { Kind = Eq } :: _ -> Error ("Assignment", List.head remaining)
        | { Kind = LParen } :: _ when splitTarget.IsNone -> Error ("Parenthesized", List.head remaining)
        | { Kind = Colon } :: _ -> Error ("No args", List.head remaining)
        | { Kind = EOF } :: _ -> Error ("No args", List.head remaining)
        | { PrecedingNewline = true } :: _ -> Error ("New line", List.head remaining)
        | [] -> Error ("No args", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
        | _ ->
            let isDebugPrint =
                match target with
                | Member (Identifier objectName, memberName) ->
                    objectName.ToLowerInvariant() = "debug" && memberName.ToLowerInvariant() = "print"
                | _ -> false
            let atEnd rem =
                match rem with
                | { Kind = EOF } :: _ | { Kind = Colon } :: _ | { PrecedingNewline = true } :: _ | [] -> true
                | { Kind = Keyword; Lexeme = "else" } :: _ -> true
                | _ -> false
            if isDebugPrint then
                // Debug.Print items separated by ';' (no space) or ',' (tab).
                let rec items acc rem =
                    if atEnd rem then Success (List.rev acc, rem)
                    else
                        match rem with
                        | { Kind = Semicolon } :: tail -> items acc tail
                        | { Kind = Comma } :: tail -> items (Literal (String "\t") :: acc) tail
                        | _ ->
                            match pexpression rem with
                            | Success (item, next) -> items (item :: acc) next
                            | Error (msg, token) -> Error (msg, token)
                // A trailing ';' or ',' continues the line at the next Debug.Print.
                let rec lastSeparator (toks: Token list) (previous: Token option) =
                    if atEnd toks then previous
                    else match toks with t :: tail -> lastSeparator tail (Some t) | [] -> previous
                let continues =
                    match lastSeparator remaining None with
                    | Some { Kind = (Semicolon | Comma) } -> true
                    | _ -> false
                let target = if continues then (match target with Member (o, _) -> Member (o, "Print;") | t -> t) else target
                match items [] remaining with
                | Success (args, rest) -> Success (ExpressionStmt (Ast.Call (target, args)), rest)
                | Error (msg, token) -> Error (msg, token)
            else
            let firstArg =
                match remaining with
                | { Kind = Comma } :: _ -> Success (MissingArg, remaining)
                | _ -> pargument remaining
            match firstArg with
            | Success (firstArg, remaining2) ->
                let rec parseMoreArgs acc rem =
                    match rem with
                    | { Kind = Comma } :: tail ->
                        match tail with
                        | { Kind = Comma } :: _ ->
                            parseMoreArgs (MissingArg :: acc) tail
                        | _ when atEnd tail ->
                            (List.rev (MissingArg :: acc), tail)
                        | _ ->
                            match pargument tail with
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

// Block scanners stop only at structural delimiters; malformed statements are
// errors, not a successful end of the enclosing list.
and pstatements (dialect: Dialect) : Parser<Statement list> = fun tokens ->
    let rec loop acc remaining =
        match skipColons remaining with
        | Error (msg, token) -> Error (msg, token)
        | Success ((), rest) ->
            match rest with
            | { Kind = Keyword; Lexeme = "end" } :: _ when isEndStatement rest ->
                match pstatement dialect rest with
                | Success (stmt, next) -> loop (stmt :: acc) next
                | Error (msg, token) -> Error (msg, token)
            | { Kind = EOF } :: _
            | { Kind = Keyword; Lexeme = ("end" | "else" | "elseif" | "next" | "wend" | "loop" | "case") } :: _ ->
                Success (List.rev acc, rest)
            | _ ->
                match pstatement dialect rest with
                | Error (msg, token) -> Error (msg, token)
                | Success (stmt, next) ->
                    match next with
                    | { Kind = Colon } :: _
                    | { Kind = EOF } :: _
                    | { PrecedingNewline = true } :: _ -> loop (stmt :: acc) next
                    | _ :: _ when (match stmt with LocatedStmt (_, LabelStmt _) -> true | _ -> false) ->
                        loop (stmt :: acc) next
                    | token :: _ -> Error ("Expected end of statement", token)
                    | [] -> Success (List.rev (stmt :: acc), next)
    loop [] tokens

and pifStmt (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "if" >>. pexpression .>> pkeyword "then") tokens with
    | Error (msg, token) -> Error (msg, token)
    | Success (cond, remaining) ->
        match remaining with
        | { PrecedingNewline = true } :: _
        | { Kind = Colon } :: _ ->
            match pstatements dialect remaining with
            | Error (msg, token) -> Error (msg, token)
            | Success (thenStmts, rest) ->
                let rec elseIfs acc rem =
                    match rem with
                    | { Kind = Keyword; Lexeme = "elseif" } :: tail ->
                        match (pexpression .>> pkeyword "then") tail with
                        | Error (msg, token) -> Error (msg, token)
                        | Success (condition, afterHeader) ->
                            match pstatements dialect afterHeader with
                            | Success (body, next) -> elseIfs ((condition, body) :: acc) next
                            | Error (msg, token) -> Error (msg, token)
                    | _ -> Success (List.rev acc, rem)
                match elseIfs [] rest with
                | Error (msg, token) -> Error (msg, token)
                | Success (branches, rest) ->
                    let elseBody =
                        match rest with
                        | { Kind = Keyword; Lexeme = "else" } :: tail ->
                            match pstatements dialect tail with
                            | Success (body, next) -> Success (Some body, next)
                            | Error (msg, token) -> Error (msg, token)
                        | _ -> Success (None, rest)
                    match elseBody with
                    | Error (msg, token) -> Error (msg, token)
                    | Success (alternative, rest) ->
                        match (skipColons >>. pkeyword "end" >>. pkeyword "if") rest with
                        | Success (_, next) -> Success (IfStmt (cond, thenStmts, branches, alternative), next)
                        | Error (msg, token) -> Error (msg, token)
        | _ ->
            // A nested inline If consumes its own Else before the enclosing branch.
            let rec branch acc rest =
                match rest with
                | { Kind = EOF } :: _
                | { PrecedingNewline = true } :: _
                | { Kind = Keyword; Lexeme = "else" } :: _ -> Success (List.rev acc, rest)
                | { Kind = Colon } :: tail -> branch acc tail
                | _ ->
                    match pstatement dialect rest with
                    | Success (stmt, next) ->
                        match next with
                        | { Kind = Colon } :: _
                        | { Kind = EOF } :: _
                        | { PrecedingNewline = true } :: _
                        | { Kind = Keyword; Lexeme = "else" } :: _ -> branch (stmt :: acc) next
                        | _ :: _ when (match stmt with LocatedStmt (_, LabelStmt _) -> true | _ -> false) ->
                            branch (stmt :: acc) next
                        | token :: _ -> Error ("Expected end of inline If statement", token)
                        | [] -> Success (List.rev (stmt :: acc), [])
                    | Error (msg, token) -> Error (msg, token)
            match branch [] remaining with
            | Error (msg, token) -> Error (msg, token)
            | Success ([], rest) -> Error ("Expected statement after Then", List.head rest)
            // An Else on a later line belongs to an enclosing block If.
            | Success (thenStmts, { Kind = Keyword; Lexeme = "else"; PrecedingNewline = false } :: tail) ->
                match branch [] tail with
                | Success ([], rest) -> Error ("Expected statement after Else", List.head rest)
                | Success (elseStmts, rest) -> Success (IfStmt (cond, thenStmts, [], Some elseStmts), rest)
                | Error (msg, token) -> Error (msg, token)
            | Success (thenStmts, rest) -> Success (IfStmt (cond, thenStmts, [], None), rest)

and pforLoop (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let var = (pkeyword "for" >>. pidentifier)
    let start = (ptoken Eq >>. pexpression)
    let end_ = (pkeyword "to" >>. pexpression)
    let step = optional (pkeyword "step" >>. pexpression)
    let body = pstatements dialect
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
                    | { Kind = Ast.Identifier; Lexeme = name; PrecedingNewline = false } :: tail when name.ToLower() = v.ToLower() -> tail
                    | _ -> remainingFinal
                Success (ForLoop (v, s, e, stepOpt, body), remainingFinal)
            | Error (msg, token) -> Error (msg, token)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pforEach (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "for" >>. pkeyword "each" >>. pidentifier .>> pkeyword "in" .>>. pexpression) tokens with
    | Success ((varName, collExpr), remaining) ->
        match (pstatements dialect .>> skipColons .>> pkeyword "next") remaining with
        | Success (body, remaining2) ->
            // `Next c`: the loop variable may follow Next.
            let remaining2 =
                match remaining2 with
                | { Kind = Ast.Identifier; Lexeme = name; PrecedingNewline = false } :: tail when name.ToLower() = varName.ToLower() -> tail
                | _ -> remaining2
            Success (ForEach (varName, collExpr, body), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pwhileLoop (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let cond = pkeyword "while" >>. pexpression
    let body = pstatements dialect
    let end_ = skipColons >>. pkeyword "wend"
    match ((cond .>>. body) .>> end_) tokens with
    | Success ((c, body), remaining) ->
        Success (WhileLoop (c, body), remaining)
    | Error (msg, token) -> Error (msg, token)

and pdoLoop (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let body = pstatements dialect

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
                Success (DoLoop (None, Some { Condition = Literal (Boolean true); Body = body }), remaining)
        | Error (msg, token) -> Error (msg, token)

    match tokens with
    | { Kind = Keyword; Lexeme = "do" } :: { Kind = Keyword; Lexeme = ("while" | "until"); PrecedingNewline = false } :: _ ->
        parseDoWhileCond tokens
    | _ -> parseLoopUntilCond tokens

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
    match (skipColons >>. pkeyword "case") tokens with
    | Error (msg, token) -> Error (msg, token)
    | Success (_, rest) ->
        let tests =
            match rest with
            | { Kind = Keyword; Lexeme = "else" } :: tail -> Success (None, tail)
            | _ ->
                match sepBy1 (ptoken Comma) pcaseTest rest with
                | Success (items, tail) -> Success (Some items, tail)
                | Error (msg, token) -> Error (msg, token)
        match tests with
        | Error (msg, token) -> Error (msg, token)
        | Success (cond, remaining) ->
            match pstatements dialect remaining with
            | Success (stmts, next) -> Success ((cond, stmts), next)
            | Error (msg, token) -> Error (msg, token)

and pselectCase (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "select" >>. pkeyword "case" >>. pexpression) tokens with
    | Error (msg, token) -> Error (msg, token)
    | Success (selector, remaining) ->
        let rec cases acc rest =
            match skipColons rest with
            | Error (msg, token) -> Error (msg, token)
            | Success ((), ({ Kind = Keyword; Lexeme = "case" } :: _ as next)) ->
                match pcase dialect next with
                | Success (clause, following) -> cases (clause :: acc) following
                | Error (msg, token) -> Error (msg, token)
            | Success ((), rest) ->
                if List.isEmpty acc then Error ("Expected Case clause", List.head rest)
                else
                    match (pkeyword "end" >>. pkeyword "select") rest with
                    | Success (_, next) -> Success (SelectCase (selector, List.rev acc), next)
                    | Error (msg, token) -> Error (msg, token)
        cases [] remaining

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
            | { Kind = Operator; Lexeme = "-" } :: { Kind = Number; Lexeme = "1" } :: tail2 ->
                Success (OnError GoToMinusOne, tail2)
            | _ when dialect = VBA ->
                match plabelName tail with
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
        match (pstatements dialect .>> skipColons .>> pkeyword "end" .>> pkeyword "with") remaining with
        | Success (body, remaining2) -> Success (WithStmt (expr, body), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and presumeStmt : Parser<Statement> = fun tokens ->
    match tokens with
    | { Kind = Keyword; Lexeme = "resume" } :: rest ->
        match rest with
        | { Kind = Keyword; Lexeme = "next"; PrecedingNewline = false } :: tail -> Success (ResumeStmt ResumeNextStatement, tail)
        | { Kind = Number; Lexeme = "0"; PrecedingNewline = false } :: tail -> Success (ResumeStmt ResumeRetry, tail)
        | { Kind = Ast.Identifier; Lexeme = label; PrecedingNewline = false } :: tail -> Success (ResumeStmt (ResumeAtLabel label), tail)
        | { Kind = Number; Lexeme = label; PrecedingNewline = false } :: tail when label |> Seq.forall System.Char.IsDigit ->
            Success (ResumeStmt (ResumeAtLabel label), tail)
        | _ -> Success (ResumeStmt ResumeRetry, rest)
    | head :: _ -> Error ("Expected Resume", head)
    | [] -> Error ("Expected Resume", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

and peraseStmt : Parser<Statement> = fun tokens ->
    match (pkeyword "erase" >>. sepBy1 (ptoken Comma) pidentifier) tokens with
    | Success (names, remaining) -> Success (EraseStmt names, remaining)
    | Error (msg, token) -> Error (msg, token)

/// A GoTo / GoSub / Resume target: a label or a line number.
and plabelName : Parser<string> = fun tokens ->
    match tokens with
    | { Kind = Number; Lexeme = lexeme } :: tail when lexeme |> Seq.forall System.Char.IsDigit -> Success (lexeme, tail)
    | _ -> pidentifier tokens

and praiseEventStmt : Parser<Statement> = fun tokens ->
    match (pkeyword "raiseevent" >>. pidentifier) tokens with
    | Success (name, ({ Kind = LParen; PrecedingNewline = false } :: _ as rest)) ->
        match pargList rest with
        | Success (args, next) -> Success (RaiseEventStmt (name, args), next)
        | Error (msg, token) -> Error (msg, token)
    | Success (name, rest) -> Success (RaiseEventStmt (name, []), rest)
    | Error (msg, token) -> Error (msg, token)

/// `LSet s = value` / `RSet s = value`: assign through a built-in.
and plsetStmt : Parser<Statement> = fun tokens ->
    match tokens with
    | { Kind = Ast.Identifier; Lexeme = keyword } :: rest ->
        let builtin = keyword.ToLowerInvariant() + ":statement"
        match ptargetAssignment rest with
        | Success (Assignment (name, value), next) ->
            Success (Assignment (name, Ast.Call (Identifier builtin, [ Identifier name; value ])), next)
        | Success (MemberAssignment (target, memberName, value, false), next) ->
            Success (MemberAssignment (target, memberName, Ast.Call (Identifier builtin, [ Member (target, memberName); value ]), false), next)
        | Success (IndexedAssignment (name, indices, value, isSet), next) ->
            Success (IndexedAssignment (name, indices, Ast.Call (Identifier builtin, [ Ast.Call (Identifier name, indices); value ]), isSet), next)
        | Success (_, _) -> Error ("Expected variable after LSet/RSet", List.head rest)
        | Error (msg, token) -> Error (msg, token)
    | head :: _ -> Error ("Expected LSet", head)
    | [] -> Error ("Expected LSet", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

and pcallStmt (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match (pkeyword "call" >>. ppostfix) tokens with
    | Success (expr, remaining) -> Success (CallStmt (expr, []), remaining)
    | Error (msg, token) -> Error (msg, token)

and pgotoStmt : Parser<Statement> = fun tokens ->
    match (pkeyword "goto" >>. plabelName) tokens with
    | Success (label, remaining) -> Success (GoToStmt label, remaining)
    | Error (msg, token) -> Error (msg, token)

and pgosubStmt : Parser<Statement> = fun tokens ->
    match (pkeyword "gosub" >>. plabelName) tokens with
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
    // A line number: `10 x = 1`, `20: y = 2`.
    | { Kind = Number; Lexeme = number } :: tail when number |> Seq.forall System.Char.IsDigit ->
        match tail with
        | { Kind = Colon } :: rest -> Success (LabelStmt number, rest)
        | _ -> Success (LabelStmt number, tail)
    | head :: _ -> Error ("Expected label", head)
    | [] -> Error ("Expected label but reached EOF", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })

and pVisibilityDecl (dialect: Dialect) : Parser<Statement> = fun tokens ->
    // Public/Private/Global Dim-like declarations (without Dim keyword)
    match parseVisibility tokens with
    | Success (vis, remaining) ->
        // Check if it's a Const
        match remaining with
        | { Kind = Keyword; Lexeme = "const" } :: tail -> pconstItems dialect (Some vis) tail
        | _ ->
            // Variable declaration: Public x, y As Integer  (or just Public x in VBS)
            match sepBy1 (ptoken Comma) (pVarDeclarator dialect) remaining with
            | Success (declarators, remaining2) ->
                Success (Declaration (Some vis, Dim declarators), remaining2)
            | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

and pexpressionStmt : Parser<Statement> = fun tokens ->
    match pexpression tokens with
    | Success (Identifier name, remaining) -> Success (CallStmt (Identifier name, []), remaining)
    | Success (Member (receiver, name), remaining) ->
        Success (ExpressionStmt (Ast.Call (Member (receiver, name), [])), remaining)
    | Success (expr, remaining) -> Success (ExpressionStmt expr, remaining)
    | Error (msg, token) -> Error (msg, token)

and ptargetAssignment : Parser<Statement> = fun tokens ->
    match ppostfix tokens with
    | Success (expr, remaining) ->
        match remaining with
        | { Kind = Eq } :: _ ->
            match (ptoken Eq >>. pexpression) remaining with
            | Success (value, remaining2) ->
                match expr with
                | Identifier name -> Success (Assignment (name, value), remaining2)
                | Member (objExpr, memberName) -> Success (MemberAssignment (objExpr, memberName, value, false), remaining2)
                | Ast.Call (Identifier name, indices) -> Success (IndexedAssignment (name, indices, value, false), remaining2)
                | Ast.Call _ -> Success (DefaultAssignment (expr, value, false), remaining2)
                | headExpr -> Error (sprintf "Expected assignable target but got %A" headExpr, List.head remaining)
            | Error (msg, token) -> Error (msg, token)
        | head :: _ -> Error ("Expected assignment", head)
        | [] -> Error ("Expected assignment", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
    | Error (msg, token) -> Error (msg, token)

and pstatement (dialect: Dialect) : Parser<Statement> = fun tokens ->
    let parse =
        if dialect = VBA then
            match plabel tokens with
            | Success _ as result -> result
            | Error _ -> pstatementAfterColons dialect tokens
        else pstatementAfterColons dialect tokens
    match parse with
    | Success (stmt, rest) ->
        let line =
            match skipColons tokens with
            | Success ((), token :: _) -> token.Line
            | _ -> (List.head tokens).Line
        Success (LocatedStmt (line, stmt), rest)
    | Error (msg, token) -> Error (msg, token)

and pstatementAfterColons (dialect: Dialect) : Parser<Statement> = fun tokens ->
    match skipColons tokens with
    | Success ((), tokens) ->
        // Try label after colons too (VBA only)
        let parseNonLabel () =
            match tokens with
            | [] -> Error ("Expected statement", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0; PrecedingNewline = false })
            | { Kind = Keyword; Lexeme = lexeme } :: _ ->
                match lexeme.ToLower() with
                | "select" -> pselectCase dialect tokens
                | "on" -> pOnError dialect tokens
                | "with" -> pwithStmt dialect tokens
                | "erase" -> peraseStmt tokens
                | "resume" -> presumeStmt tokens
                | "redim" -> predim tokens
                | "dim" -> pdim dialect tokens
                | "static" -> pdimWith "static" dialect tokens
                | "raiseevent" -> praiseEventStmt tokens
                | "end" when isEndStatement tokens -> Success (EndStmt, List.tail tokens)
                | "const" -> pconst dialect tokens
                | "set" -> psetStmtFull tokens
                | "let" -> (pkeyword "let" >>. ptargetAssignment) tokens
                | "if" -> pifStmt dialect tokens
                | "for" ->
                    match tokens with
                    | _ :: { Kind = Keyword; Lexeme = "each" } :: _ -> pforEach dialect tokens
                    | _ -> pforLoop dialect tokens
                | "while" -> pwhileLoop dialect tokens
                | "do" -> pdoLoop dialect tokens
                | "exit" -> pexit tokens
                | "call" -> pcallStmt dialect tokens
                | "goto" -> pgotoStmt tokens
                | "gosub" -> pgosubStmt tokens
                | "return" -> preturnStmt tokens
                | "public" | "private" | "global" -> pVisibilityDecl dialect tokens
                | _ -> pexpressionStmt tokens
            | { Kind = Ast.Identifier; Lexeme = keyword } :: { Kind = Ast.Identifier; PrecedingNewline = false } :: _
                when (let k = keyword.ToLowerInvariant() in k = "lset" || k = "rset") ->
                plsetStmt tokens
            | { Kind = Ast.Identifier } :: _
            | { Kind = BracketText } :: _
            | { Kind = Dot } :: _ ->
                (ptargetAssignment
                 <|> pSubCallNoParens
                 <|> pexpressionStmt) tokens
            | _ -> pexpressionStmt tokens

        if dialect = VBA then
            match plabel tokens with
            | Success _ as result -> result
            | Error _ -> parseNonLabel ()
        else
            parseNonLabel ()
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
                            | Success (_, { Kind = LParen; PrecedingNewline = false } :: { Kind = RParen } :: rem) -> (None, rem)
                            | Success (tr, rem) -> (Some tr, rem)
                            | Error _ -> (None, remaining2)
                        | _ -> (None, remaining2)
                    | VBScript -> (None, remaining2)
                match (pstatements dialect .>> skipColons .>> pkeyword "end" .>> pkeyword "function") remaining3 with
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
                match (pstatements dialect .>> skipColons .>> pkeyword "end" .>> pkeyword "sub") remaining2 with
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
                                    | Success (_, { Kind = LParen; PrecedingNewline = false } :: { Kind = RParen } :: rem) -> (None, rem)
                                    | Success (tr, rem) -> (Some tr, rem)
                                    | Error _ -> (None, remaining3)
                                | _ -> (None, remaining3)
                            | VBScript -> (None, remaining3)
                        match (pstatements dialect .>> skipColons .>> pkeyword "end" .>> pkeyword "property") remaining4 with
                        | Success (body, remaining5) ->
                            Success (PropertyGet (mods, name, parameters, returnType, body), remaining5)
                        | Error (msg, token) -> Error (msg, token)
                    | "let" ->
                        match (pstatements dialect .>> skipColons .>> pkeyword "end" .>> pkeyword "property") remaining3 with
                        | Success (body, remaining4) ->
                            Success (PropertyLet (mods, name, parameters, body), remaining4)
                        | Error (msg, token) -> Error (msg, token)
                    | "set" ->
                        match (pstatements dialect .>> skipColons .>> pkeyword "end" .>> pkeyword "property") remaining3 with
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
    // `Dim WithEvents x As C` is the same as `Private WithEvents x As C`.
    let tokens = match tokens with { Kind = Keyword; Lexeme = "dim" } :: ({ Kind = Keyword; Lexeme = "withevents" } :: _ as rest) -> rest | _ -> tokens
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
    | Success (_, option :: value :: tail)
        when option.Lexeme.ToLowerInvariant() = "compare" && not option.PrecedingNewline &&
             not value.PrecedingNewline && (value.Lexeme.ToLowerInvariant() = "text" || value.Lexeme.ToLowerInvariant() = "binary") ->
        Success (OptionStmt ("compare " + value.Lexeme.ToLowerInvariant()), tail)
    | Success (_, option :: { Kind = Number; Lexeme = ("0" | "1") as value } :: tail)
        when option.Lexeme.ToLowerInvariant() = "base" && not option.PrecedingNewline ->
        Success (OptionStmt ("base " + value), tail)
    | Success (_, { Kind = Keyword; Lexeme = "private" } :: m :: tail) when m.Lexeme.ToLowerInvariant() = "module" ->
        Success (OptionStmt "private module", tail)
    | Success (_, option :: tail) when option.Lexeme.ToLowerInvariant() = "compare" ->
        Error ("Expected Text or Binary after Option Compare", List.head tail)
    | Success (_, { Kind = Keyword; Lexeme = lexeme } :: tail)
    | Success (_, { Kind = Ast.Identifier; Lexeme = lexeme } :: tail) ->
        Success (OptionStmt lexeme, tail)
    | Success (_, head :: _) -> Error ("Expected option name after 'Option'", head)
    | Success (_, []) -> Error ("Expected option name after 'Option'", List.head tokens)
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
                // Once a declaration head is seen, do not backtrack into an
                // unrelated statement and lose the declaration's diagnostic.
                match topLevelHeadAfterModifiers remaining with
                | { Kind = Keyword; Lexeme = ("function" | "sub" | "property") } :: _ ->
                    match pfunction dialect remaining with
                    | Success (func, remaining2) -> parseMembers (FunctionDef func :: acc) remaining2
                    | Error (msg, token) -> Error (msg, token)
                | _ ->
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
        let statement () = (pstatement dialect |>> TopLevelStatement) tokens
        match topLevelHeadAfterModifiers tokens with
        | { Kind = Keyword; Lexeme = lexeme } :: _ ->
            match lexeme.ToLower() with
            | "class" -> pclass dialect tokens
            | "function" | "sub" | "property" -> (pfunction dialect |>> FunctionDef) tokens
            | "option" -> poption tokens
            | "enum" when dialect = VBA -> penum tokens
            | "type" when dialect = VBA -> ptypeDecl tokens
            | "implements" when dialect = VBA -> pimplements tokens
            | "declare" when dialect = VBA -> pdeclare dialect tokens
            | "event" when dialect = VBA -> peventDecl dialect tokens
            | "withevents" when dialect = VBA -> pWithEventsDecl dialect tokens
            | "dim" when dialect = VBA && (match tokens with _ :: { Kind = Keyword; Lexeme = "withevents" } :: _ -> true | _ -> false) ->
                pWithEventsDecl dialect tokens
            | _ -> statement ()
        | _ -> statement ()
    | Error (msg, token) -> Error (msg, token)

/// Expand statement groups into their statements, keeping each one's line.
let rec private flattenStatements (statements: Statement list) : Statement list =
    statements |> List.collect (fun stmt ->
        match stmt with
        | StatementGroup inner -> flattenStatements inner
        | LocatedStmt (line, StatementGroup inner) -> flattenStatements inner |> List.map (fun s -> match s with LocatedStmt _ -> s | _ -> LocatedStmt (line, s))
        | LocatedStmt (line, inner) -> [ LocatedStmt (line, flattenStatement inner) ]
        | other -> [ flattenStatement other ])

and private flattenStatement (stmt: Statement) : Statement =
    match stmt with
    | IfStmt (c, yes, alternatives, no) ->
        IfStmt (c, flattenStatements yes, alternatives |> List.map (fun (e, b) -> e, flattenStatements b), no |> Option.map flattenStatements)
    | SelectCase (e, cases) -> SelectCase (e, cases |> List.map (fun (t, b) -> t, flattenStatements b))
    | ForLoop (v, a, b, step, body) -> ForLoop (v, a, b, step, flattenStatements body)
    | ForEach (v, e, body) -> ForEach (v, e, flattenStatements body)
    | WhileLoop (c, body) -> WhileLoop (c, flattenStatements body)
    | DoLoop (a, b) ->
        let f (c: DoCondition option) = c |> Option.map (fun c -> { c with Body = flattenStatements c.Body })
        DoLoop (f a, f b)
    | WithStmt (e, body) -> WithStmt (e, flattenStatements body)
    | LocatedStmt (line, inner) -> LocatedStmt (line, flattenStatement inner)
    | other -> other

let rec private flattenTopLevels (tops: TopLevel list) : TopLevel list =
    let flattenFunction f =
        match f with
        | FunctionDecl (m, n, ps, t, body) -> FunctionDecl (m, n, ps, t, flattenStatements body)
        | SubDecl (m, n, ps, body) -> SubDecl (m, n, ps, flattenStatements body)
        | PropertyGet (m, n, ps, t, body) -> PropertyGet (m, n, ps, t, flattenStatements body)
        | PropertyLet (m, n, ps, body) -> PropertyLet (m, n, ps, flattenStatements body)
        | PropertySet (m, n, ps, body) -> PropertySet (m, n, ps, flattenStatements body)
    tops |> List.collect (fun top ->
        match top with
        | TopLevelStatement stmt -> flattenStatements [ stmt ] |> List.map TopLevelStatement
        | FunctionDef f -> [ FunctionDef (flattenFunction f) ]
        | ClassDecl (v, n, members) -> [ ClassDecl (v, n, flattenTopLevels members) ]
        | other -> [ other ])

let pprogram (dialect: Dialect) : Parser<Program> = fun tokens ->
    let rec loop acc remaining =
        match skipColons remaining with
        | Error (msg, token) -> Error (msg, token)
        | Success ((), ({ Kind = EOF } :: _ as rest)) ->
            Success ({ Dialect = dialect; TopLevels = flattenTopLevels (List.rev acc) }, rest)
        | Success ((), rest) ->
            match pTopLevel dialect rest with
            | Error (msg, token) -> Error (msg, token)
            | Success (topLevel, next) ->
                match next with
                | { Kind = Colon } :: _
                | { Kind = EOF } :: _
                | { PrecedingNewline = true } :: _ -> loop (topLevel :: acc) next
                | _ :: _ when (match topLevel with TopLevelStatement (LocatedStmt (_, LabelStmt _)) -> true | _ -> false) ->
                    loop (topLevel :: acc) next
                | token :: _ -> Error ("Expected end of statement", token)
                | [] -> Error ("Expected end of input", List.head rest)
    loop [] tokens

let private parseSource dialect source =
    try
        match pprogram dialect (Lexer.tokenize source) with
        | Success (program, { Kind = EOF } :: _) -> Ok program
        | Success (_, token :: _) -> Result.Error (sprintf "Parse error at line %d, column %d: Expected end of input" token.Line token.Column)
        | Success (_, []) -> Result.Error "Parse error: Expected end of input"
        | Error (msg, token) -> Result.Error (sprintf "Parse error at line %d, column %d: %s" token.Line token.Column msg)
    with ex ->
        Result.Error (sprintf "Parse error: %s" ex.Message)

/// Parse returning (message, line, column) on failure.
let parseDetailed (dialect: Dialect) (source: string) : Result<Program, string * int * int> =
    try
        match pprogram dialect (Lexer.tokenize source) with
        | Success (program, { Kind = EOF } :: _) -> Ok program
        | Success (_, token :: _) -> Result.Error ("Expected end of input", token.Line, token.Column)
        | Success (_, []) -> Result.Error ("Expected end of input", 0, 0)
        | Error (msg, token) -> Result.Error (msg, token.Line, token.Column)
    with ex ->
        Result.Error (ex.Message, 0, 0)

/// Parse a standalone expression (for an Immediate window `?expr`).
let parseExpression (source: string) : Result<Expression, string * int * int> =
    try
        match pexpression (Lexer.tokenize source) with
        | Success (expr, { Kind = EOF } :: _) -> Ok expr
        | Success (_, token :: _) -> Result.Error ("Expected end of expression", token.Line, token.Column)
        | Success (_, []) -> Ok (Literal Empty)
        | Error (msg, token) -> Result.Error (msg, token.Line, token.Column)
    with ex ->
        Result.Error (ex.Message, 0, 0)

/// Strict mode: parse only complete input with the selected dialect grammar.
let parse_dialect (dialect: Dialect) (source: string) : Result<Program, string> =
    parseSource dialect source

/// Tolerant mode: parse as VBA superset, tag with target dialect for validation.
let parse_tolerant (dialect: Dialect) (source: string) : Result<Program, string> =
    parseSource VBA source |> Result.map (fun program -> { program with Dialect = dialect })

let parse (source: string) : Result<Program, string> =
    parse_dialect VBScript source
