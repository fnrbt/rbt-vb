module Parser

open Ast
open Lexer

type ParseResult<'a> =
    | Success of 'a * Token list
    | Error of string * Token

type Parser<'a> = Token list -> ParseResult<'a>

let (<!>) (f: Parser<'a>) (name: string) (tokens: Token list) =
    match f tokens with
    | Success (result, remaining) -> Success (result, remaining)
    | Error (msg, token) -> Error (sprintf "%s: %s" name msg, token)

let (>>=) (parser: Parser<'a>) (f: 'a -> Parser<'b>) (tokens: Token list) =
    match parser tokens with
    | Success (result, remaining) -> f result remaining
    | Error (msg, token) -> Error (msg, token)

let (|>>) (parser: Parser<'a>) (f: 'a -> 'b) (tokens: Token list) =
    match parser tokens with
    | Success (result, remaining) -> Success (f result, remaining)
    | Error (msg, token) -> Error (msg, token)

let (>>%) (parser: Parser<'a>) (value: 'b) (tokens: Token list) =
    match parser tokens with
    | Success (_, remaining) -> Success (value, remaining)
    | Error (msg, token) -> Error (msg, token)

let (<|>) (p1: Parser<'a>) (p2: Parser<'a>) (tokens: Token list) =
    match p1 tokens with
    | Success _ as result -> result
    | Error _ -> p2 tokens

let (.>>.) (p1: Parser<'a>) (p2: Parser<'b>) (tokens: Token list) =
    match p1 tokens with
    | Success (result1, remaining1) ->
        match p2 remaining1 with
        | Success (result2, remaining2) -> Success ((result1, result2), remaining2)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let (.>>) (p1: Parser<'a>) (p2: Parser<'b>) (tokens: Token list) =
    (p1 .>>. p2) |>> fst <| tokens

let (>>.) (p1: Parser<'a>) (p2: Parser<'b>) (tokens: Token list) =
    (p1 .>>. p2) |>> snd <| tokens

let between (left: Parser<'a>) (right: Parser<'b>) (center: Parser<'c>) (tokens: Token list) =
    (left >>. center .>> right) <| tokens

let optional (parser: Parser<'a>) (tokens: Token list) =
    match parser tokens with
    | Success (result, remaining) -> Success (Some result, remaining)
    | Error _ -> Success (None, tokens)

let many (parser: Parser<'a>) (tokens: Token list) =
    let rec loop acc remaining =
        match parser remaining with
        | Success (result, newRemaining) -> loop (result :: acc) newRemaining
        | Error _ -> Success (List.rev acc, remaining)
    loop [] tokens

let many1 (parser: Parser<'a>) (tokens: Token list) =
    match parser tokens with
    | Success (result, remaining) ->
        match many parser remaining with
        | Success (results, finalRemaining) -> Success (result :: results, finalRemaining)
        | Error (msg, token) -> Error (msg, token)
    | Error (msg, token) -> Error (msg, token)

let sepBy (separator: Parser<'a>) (parser: Parser<'b>) (tokens: Token list) =
    let rec loop acc remaining =
        match parser remaining with
        | Success (result, newRemaining) ->
            match separator newRemaining with
            | Success (_, newerRemaining) -> loop (result :: acc) newerRemaining
            | Error _ -> Success (List.rev (result :: acc), newRemaining)
        | Error _ -> Success (List.rev acc, remaining)
    loop [] tokens

let sepBy1 (separator: Parser<'a>) (parser: Parser<'b>) (tokens: Token list) =
    match parser tokens with
    | Success (result, remaining) ->
        let rec loop acc remaining =
            match separator remaining with
            | Success (_, newRemaining) ->
                match parser newRemaining with
                | Success (result', newerRemaining) -> loop (result' :: acc) newerRemaining
                | Error _ -> Success (List.rev acc, newRemaining)
            | Error _ -> Success (List.rev acc, remaining)
        loop [result] remaining
    | Error (msg, token) -> Error (msg, token)

let ptoken (kind: TokenKind) (tokens: Token list) =
    match tokens with
    | { Kind = k } :: tail when k = kind -> Success (List.head tokens, tail)
    | head :: _ -> Error (sprintf "Expected token of kind %A but got %A" kind head.Kind, head)
    | [] -> Error (sprintf "Expected token of kind %A but reached EOF" kind, { Kind = EOF; Lexeme = ""; Line = 0; Column = 0 })

let pkeyword (keyword: string) (tokens: Token list) =
    match tokens with
    | { Kind = Keyword; Lexeme = lexeme } :: tail when lexeme.ToLower() = keyword.ToLower() -> Success (List.head tokens, tail)
    | { Kind = Keyword; Lexeme = lexeme } :: _ -> 
        Error (sprintf "Expected keyword '%s' but got '%s'" keyword lexeme, List.head tokens)
    | head :: _ -> Error (sprintf "Expected keyword '%s' but got %A" keyword head.Kind, head)
    | [] -> Error (sprintf "Expected keyword '%s' but reached EOF" keyword, { Kind = EOF; Lexeme = ""; Line = 0; Column = 0 })

let pidentifier (tokens: Token list) =
    match tokens with
    | { Kind = Ast.Identifier; Lexeme = lexeme } :: tail -> Success (lexeme, tail)
    | head :: _ -> Error (sprintf "Expected identifier but got %A" head.Kind, head)
    | [] -> Error ("Expected identifier but reached EOF", { Kind = Ast.EOF; Lexeme = ""; Line = 0; Column = 0 })

let pnumber (tokens: Token list) =
    match tokens with
    | { Kind = Number; Lexeme = lexeme } :: tail ->
        let value = 
            if lexeme.Contains "." 
            then Double (float lexeme) 
            else Integer (int lexeme)
        Success (value, tail)
    | head :: _ -> Error (sprintf "Expected number but got %A" head.Kind, head)
    | [] -> Error ("Expected number but reached EOF", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0 })

let pstring (tokens: Token list) =
    match tokens with
    | { Kind = StringLiteral; Lexeme = lexeme } :: tail -> Success (String lexeme, tail)
    | head :: _ -> Error (sprintf "Expected string but got %A" head.Kind, head)
    | [] -> Error ("Expected string but reached EOF", { Kind = EOF; Lexeme = ""; Line = 0; Column = 0 })

let poperator (op: string) (tokens: Token list) =
    match tokens with
    | { Kind = Operator; Lexeme = lexeme } :: tail when lexeme = op -> Success ((), tail)
    | { Kind = Operator; Lexeme = lexeme } :: _ -> 
        Error (sprintf "Expected operator '%s' but got '%s'" op lexeme, List.head tokens)
    | head :: _ -> Error (sprintf "Expected operator '%s' but got %A" op head.Kind, head)
    | [] -> Error (sprintf "Expected operator '%s' but reached EOF" op, { Kind = EOF; Lexeme = ""; Line = 0; Column = 0 })

let rec pprimary (tokens: Token list) =
    let lit = pliteral
    let ident = pidentifier |>> Identifier
    let paren = between (ptoken LParen) (ptoken RParen) pexpression
    
    let parseCall tokens =
        match pidentifier tokens with
        | Success (name, remaining) ->
            match remaining with
            | { Kind = LParen } :: tail ->
                let rec parseArgs acc remaining =
                    match remaining with
                    | { Kind = RParen } :: tail -> Success (List.rev acc, tail)
                    | _ ->
                        match pexpression remaining with
                        | Success (expr, newRemaining) ->
                            match newRemaining with
                            | { Kind = Comma } :: tail2 ->
                                parseArgs (expr :: acc) tail2
                            | { Kind = RParen } :: tail2 -> Success (List.rev (expr :: acc), tail2)
                            | _ -> Error ("Expected comma or closing parenthesis", List.head remaining)
                        | Error (msg, token) -> Error (msg, token)
                match parseArgs [] tail with
                | Success (args, finalRemaining) -> Success (Call (name, args), finalRemaining)
                | Error (msg, token) -> Error (msg, token)
            | _ -> Error ("Expected '(' after identifier for function call", List.head remaining)
        | Error (msg, token) -> Error (msg, token)
    
    parseCall <|> lit <|> ident <|> paren <| tokens

and pliteral (tokens: Token list) =
    let num = pnumber |>> fun v -> 
        match v with
        | Integer i -> Literal (Integer i)
        | Double d -> Literal (Double d)
        | _ -> Literal (Integer 0)
    let str = pstring |>> fun v -> 
        match v with
        | Ast.String s -> Literal (Ast.String s)
        | _ -> Literal (Ast.Integer 0)
    let true_ = pkeyword "true" >>% Literal (Boolean true)
    let false_ = pkeyword "false" >>% Literal (Boolean false)
    let null_ = pkeyword "null" >>% Literal Null
    let empty_ = pkeyword "empty" >>% Literal Empty
    true_ <|> false_ <|> null_ <|> empty_ <|> str <|> num <| tokens

and punary (tokens: Token list) =
    let not_ = pkeyword "not" >>. punary |>> fun e -> Unary (Not, e)
    let neg = poperator "-" >>. punary |>> fun e -> Unary (Negate, e)
    let pos = poperator "+" >>. punary
    let primary = pprimary
    not_ <|> neg <|> pos <|> primary <| tokens

and pterm (tokens: Token list) =
    let rec parseTermLeft tokens =
        match punary tokens with
        | Error (msg, token) -> Error (msg, token)
        | Success (leftExpr, remaining) ->
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
                    | Success (rightExpr, newerRemaining) -> loop (Binary (Divide, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | { Kind = Keyword; Lexeme = "mod" } :: tail ->
                    match punary tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (Modulus, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | _ -> Success (acc, remaining)
            loop leftExpr remaining
    parseTermLeft tokens

and pfactor (tokens: Token list) =
    let rec parseFactorLeft tokens =
        match pterm tokens with
        | Error (msg, token) -> Error (msg, token)
        | Success (leftExpr, remaining) ->
            let rec loop acc remaining =
                match remaining with
                | { Kind = Operator; Lexeme = "^" } :: tail ->
                    match pfactor tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (Power, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | _ -> Success (acc, remaining)
            loop leftExpr remaining
    parseFactorLeft tokens

and pconcat (tokens: Token list) =
    let rec parseConcatLeft tokens =
        match pfactor tokens with
        | Error (msg, token) -> Error (msg, token)
        | Success (leftExpr, remaining) ->
            let rec loop acc remaining =
                match remaining with
                | { Kind = Operator; Lexeme = "&" } :: tail ->
                    match pfactor tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (Concatenate, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | _ -> Success (acc, remaining)
            loop leftExpr remaining
    parseConcatLeft tokens

and pcomparison (tokens: Token list) =
    let rec parseComparisonLeft tokens =
        match pconcat tokens with
        | Error (msg, token) -> Error (msg, token)
        | Success (leftExpr, remaining) ->
            let rec loop acc remaining =
                match remaining with
                | { Kind = Operator; Lexeme = "=" } :: tail ->
                    match pconcat tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (Equal, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | { Kind = Operator; Lexeme = "<>" } :: tail ->
                    match pconcat tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (NotEqual, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | { Kind = Operator; Lexeme = "<" } :: tail ->
                    match pconcat tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (LessThan, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | { Kind = Operator; Lexeme = "<=" } :: tail ->
                    match pconcat tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (LessEqual, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | { Kind = Operator; Lexeme = ">" } :: tail ->
                    match pconcat tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (GreaterThan, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | { Kind = Operator; Lexeme = ">=" } :: tail ->
                    match pconcat tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (GreaterEqual, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | _ -> Success (acc, remaining)
            loop leftExpr remaining
    parseComparisonLeft tokens

and pand (tokens: Token list) =
    let rec parseAndLeft tokens =
        match pcomparison tokens with
        | Error (msg, token) -> Error (msg, token)
        | Success (leftExpr, remaining) ->
            let rec loop acc remaining =
                match remaining with
                | { Kind = Keyword; Lexeme = "and" } :: tail ->
                    match pcomparison tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (And, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | _ -> Success (acc, remaining)
            loop leftExpr remaining
    parseAndLeft tokens

and por (tokens: Token list) =
    let rec parseOrLeft tokens =
        match pand tokens with
        | Error (msg, token) -> Error (msg, token)
        | Success (leftExpr, remaining) ->
            let rec loop acc remaining =
                match remaining with
                | { Kind = Keyword; Lexeme = "or" } :: tail ->
                    match pand tail with
                    | Success (rightExpr, newerRemaining) -> loop (Binary (Or, acc, rightExpr)) newerRemaining
                    | Error (msg, token) -> Error (msg, token)
                | _ -> Success (acc, remaining)
            loop leftExpr remaining
    parseOrLeft tokens

and pexpression (tokens: Token list) =
    parseOrLeft tokens

let rec pstatement (tokens: Token list) =
    pifStmt <| pdim <|> pletStmt <|> pforLoop <|> pwhileLoop <|> pexpressionStmt <| tokens

and pstatementList (tokens: Token list) =
    many pstatement <| tokens

and pifStmt (tokens: Token list) =
    let condition = pkeyword "if" >>. pexpression .>> pkeyword "then"
    let thenBlock = pstatementList
    let elseBlock = optional (pkeyword "else" >>. pstatementList)
    let end_ = pkeyword "end" >>. pkeyword "if"
    (condition .>>. thenBlock .>>. elseBlock .>> end_) |>> function
        | ((cond, thenStmts), elseStmts) -> IfStmt (cond, thenStmts, elseStmts)
    <| tokens

and pdim (tokens: Token list) =
    let name = pkeyword "dim" >>. pidentifier
    let init = optional (poperator "=" >>. pexpression)
    (name .>>. init) |>> function
        | (n, Some expr) -> Declaration (Dim (n, Some expr))
        | (n, None) -> Declaration (Dim (n, None))
    <| tokens

and pletStmt (tokens: Token list) =
    let name = pidentifier
    let expr = poperator "=" >>. pexpression
    (name .>>. expr) |>> Assignment <| tokens

and pforLoop (tokens: Token list) =
    let var = pkeyword "for" >>. pidentifier
    let start = poperator "=" >>. pexpression
    let end_ = pkeyword "to" >>. pexpression
    let step = optional (pkeyword "step" >>. pexpression)
    let body = pstatementList
    let next = pkeyword "next" >>. optional pidentifier
    (var .>>. start .>>. end_ .>>. step .>>. body .>>. next) |>> function
        | (((v, s), e), st, body, _) -> ForLoop (v, s, e, st, body)
    <| tokens

and pwhileLoop (tokens: Token list) =
    let cond = pkeyword "while" >>. pexpression
    let body = pstatementList
    let end_ = pkeyword "wend"
    (cond .>>. body .>> end_) |>> WhileLoop <| tokens

and pexpressionStmt (tokens: Token list) =
    pexpression |>> ExpressionStmt <| tokens

let rec pfunction (tokens: Token list) =
    let func = (pkeyword "function" >>. pidentifier .>>. ptoken LParen .>>. optional (pidentifier |> sepBy (ptoken Comma)) .>>. ptoken RParen .>>. pstatementList .>>. pkeyword "end" .>>. pkeyword "function") |>> function
               | ((name, _), Some params_) -> FunctionDecl (name, params_ |> List.map (fun p -> { Name = p; ByRef = false; Optional = false; DefaultValue = None }))
               | ((name, _), None) -> FunctionDecl (name, [])
    func <| tokens

let rec pprogram (tokens: Token list) =
    let funcs = many pfunction
    let stmts = pstatementList
    (funcs .>>. stmts) |>> Program <| tokens

let parse (source: string) =
    let tokens = Lexer.tokenize source
    match pprogram tokens with
    | Success (program, _) -> Ok program
    | Error (msg, token) -> Error (sprintf "Parse error at line %d, column %d: %s" token.Line token.Column msg)
