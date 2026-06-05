module Lexer

open Ast

let keywords = set [
    "dim"; "redim"; "if"; "then"; "else"; "elseif"; "end"; "select"; "case"
    "for"; "to"; "step"; "next"; "each"; "while"; "wend"; "do"; "loop"; "until"
    "exit"; "function"; "sub"; "call"; "byref"; "byval"; "optional"
    "true"; "false"; "null"; "empty"; "nothing"
    "and"; "or"; "not"; "xor"; "eqv"; "imp"; "mod"; "is"; "like"
    "class"; "new"; "set"; "let"; "in"; "on"; "error"; "resume"; "const"
    "private"; "public"; "friend"; "global"; "preserve"; "static"; "default"
    "property"; "get"; "with"; "goto"; "as"
    "enum"; "type"; "declare"; "implements"; "option"; "explicit"
    "paramarray"; "erase"; "event"; "withevents"; "raiseevent"; "lib"; "alias"
    "typeof"; "gosub"; "return"; "me"
]

let private isDigit c = c >= '0' && c <= '9'
let private isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let private isAlphaNumeric c = isAlpha c || isDigit c
let private isWhitespace c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

type LexerState = {
    Source: string
    Position: int
    Line: int
    Column: int
}

let private advance (state: LexerState) =
    let c = if state.Position < state.Source.Length then state.Source.[state.Position] else '\000'
    let newLine = c = '\n'
    let newLineNum = if newLine then state.Line + 1 else state.Line
    let newCol = if newLine then 1 else state.Column + 1
    { state with Position = state.Position + 1; Line = newLineNum; Column = newCol }

let private peek (state: LexerState) =
    if state.Position < state.Source.Length then state.Source.[state.Position] else '\000'

let private peekN (state: LexerState) n =
    if state.Position + n < state.Source.Length
    then state.Source.[state.Position + n]
    else '\000'

let rec private skipWhitespaceTracking (state: LexerState) (sawNewline: bool) =
    let c = peek state
    if c = ' ' || c = '\t' || c = '\r' then
        skipWhitespaceTracking (advance state) sawNewline
    elif c = '\n' then
        skipWhitespaceTracking (advance state) true
    else
        (state, sawNewline)

let rec private skipLineComment (state: LexerState) =
    let c = peek state
    if c <> '\n' && c <> '\000' then
        skipLineComment (advance state)
    else
        state

let private lexNumber (state: LexerState) =
    let rec loop (acc: char list) (st: LexerState) =
        let c = peek st
        if isDigit c || c = '.' then
            loop (c :: acc) (advance st)
        else
            let lexeme = new string (List.rev acc |> List.toArray)
            let value = if lexeme.Contains "."
                        then Double (float lexeme)
                        else Integer (int lexeme)
            (value, st)
    loop [] state

let private lexString (state: LexerState) =
    let stateAfterOpenQuote = advance state
    let startLine = stateAfterOpenQuote.Line
    let startCol = stateAfterOpenQuote.Column + 1
    let rec loop (acc: char list) (st: LexerState) =
        let c = peek st
        if c = '"' then
            // Check for escaped quote ""
            if peekN st 1 = '"' then
                loop ('"' :: acc) (advance (advance st))
            else
                (acc, st)
        elif c = '\000' then
            failwithf "Unterminated string literal starting at line %d, column %d" startLine startCol
        else
            loop (c :: acc) (advance st)
    let (chars, stateAtClosingQuote) = loop [] stateAfterOpenQuote
    let finalState = advance stateAtClosingQuote
    let lexeme = new string (List.rev chars |> List.toArray)
    (String lexeme, finalState)

let private lexIdentifier (state: LexerState) =
    let rec loop (acc: char list) (st: LexerState) =
        let c = peek st
        if isAlphaNumeric c then
            loop (c :: acc) (advance st)
        else
            let lexeme = new string (List.rev acc |> List.toArray)
            let lower = lexeme.ToLower()
            let isKeyword = Set.contains lower keywords
            // Normalize keyword lexemes to lowercase for consistent pattern matching
            let normalizedLexeme = if isKeyword then lower else lexeme
            (normalizedLexeme, isKeyword, st)
    loop [] state

let rec private lex (state: LexerState) (precedingNewline: bool) =
    let (state, hadNewline) = skipWhitespaceTracking state precedingNewline
    let nl = hadNewline
    let c = peek state
    let line = state.Line
    let col = state.Column
    let tok kind lexeme = { Kind = kind; Lexeme = lexeme; Line = line; Column = col; PrecedingNewline = nl }

    match c with
    | '\000' -> []
    | '\'' ->
        let state = skipLineComment (advance state)
        lex state true  // comment implies newline
    | '"' ->
        let (lit, newState) = lexString state
        let lexeme = match lit with String s -> s | _ -> ""
        (tok StringLiteral lexeme) :: lex newState false
    | c when isDigit c ->
        let (lit, newState) = lexNumber state
        let lexeme = match lit with Integer i -> string i | Double d -> string d | _ -> ""
        (tok Number lexeme) :: lex newState false
    | c when isAlpha c ->
        let (lexeme, isKeyword, newState) = lexIdentifier state
        if lexeme = "_" && not isKeyword then
            lex newState true  // line continuation implies newline consumed
        else
        let kind = if isKeyword then Ast.Keyword else Ast.Identifier
        (tok kind lexeme) :: lex newState false
    | '(' -> (tok LParen "(") :: lex (advance state) false
    | ')' -> (tok RParen ")") :: lex (advance state) false
    | ',' -> (tok Comma ",") :: lex (advance state) false
    | ':' -> (tok Colon ":") :: lex (advance state) false
    | ';' -> (tok Semicolon ";") :: lex (advance state) false
    | '.' -> (tok Dot ".") :: lex (advance state) false
    | '=' -> (tok Eq "=") :: lex (advance state) false
    | '+' -> (tok Operator "+") :: lex (advance state) false
    | '-' -> (tok Operator "-") :: lex (advance state) false
    | '*' -> (tok Operator "*") :: lex (advance state) false
    | '/' -> (tok Operator "/") :: lex (advance state) false
    | '\\' -> (tok Operator "\\") :: lex (advance state) false
    | '^' -> (tok Operator "^") :: lex (advance state) false
    | '<' ->
        let c2 = peekN state 1
        if c2 = '>' then (tok Operator "<>") :: lex (advance (advance state)) false
        elif c2 = '=' then (tok Operator "<=") :: lex (advance (advance state)) false
        else (tok Operator "<") :: lex (advance state) false
    | '>' ->
        let c2 = peekN state 1
        if c2 = '=' then (tok Operator ">=") :: lex (advance (advance state)) false
        else (tok Operator ">") :: lex (advance state) false
    | '&' -> (tok Operator "&") :: lex (advance state) false
    | _ -> (tok Operator (string c)) :: lex (advance state) false

let tokenize (source: string) =
    let initialState = { Source = source; Position = 0; Line = 1; Column = 1 }
    let tokens = lex initialState false
    tokens @ [{ Kind = EOF; Lexeme = ""; Line = initialState.Line; Column = initialState.Column; PrecedingNewline = false }]
