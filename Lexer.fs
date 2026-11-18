module Rbt.Vb.Lexer

open Ast

let private resizeArrayToList (items: ResizeArray<'T>) =
    let mutable result = []
    for i = items.Count - 1 downto 0 do
        result <- items.[i] :: result
    result

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

let private skipWhitespaceTracking (state: LexerState) (sawNewline: bool) =
    let mutable st = state
    let mutable foundNewline = sawNewline
    let mutable scanning = true
    while scanning do
        let c = peek st
        if c = ' ' || c = '\t' || c = '\r' then
            st <- advance st
        elif c = '\n' then
            st <- advance st
            foundNewline <- true
        else
            scanning <- false
    (st, foundNewline)

let private skipLineComment (state: LexerState) =
    let mutable st = state
    while peek st <> '\n' && peek st <> '\000' do
        st <- advance st
    st

let private isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let private isTypeSuffix c = c = '&' || c = '%' || c = '#' || c = '!' || c = '@'

/// Decimal, exponent and type-suffix literals: 12, 1.5, 1E3, 2.5E-3, 5&, 5#.
let private lexNumber (state: LexerState) =
    let mutable st = state
    while isDigit (peek st) || peek st = '.' do
        st <- advance st
    if (peek st = 'e' || peek st = 'E') &&
       (isDigit (peekN st 1) || ((peekN st 1 = '+' || peekN st 1 = '-') && isDigit (peekN st 2))) then
        st <- advance st
        if peek st = '+' || peek st = '-' then st <- advance st
        while isDigit (peek st) do st <- advance st
    if isTypeSuffix (peek st) && not (isAlphaNumeric (peekN st 1)) && peekN st 1 <> '"' then
        st <- advance st
    let lexeme = state.Source.Substring(state.Position, st.Position - state.Position)
    (lexeme, st)

/// `&H1F`, `&O17` (optionally with a `&` / `%` type suffix).
let private tryLexRadix (state: LexerState) =
    let marker = peekN state 1
    let isHex = marker = 'h' || marker = 'H'
    let isOct = marker = 'o' || marker = 'O'
    let digitOk c = if isHex then isHexDigit c else c >= '0' && c <= '7'
    if (isHex || isOct) && digitOk (peekN state 2) then
        let mutable st = advance (advance state)
        while digitOk (peek st) do st <- advance st
        if (peek st = '&' || peek st = '%') && not (isAlphaNumeric (peekN st 1)) then st <- advance st
        Some (state.Source.Substring(state.Position, st.Position - state.Position), st)
    else None

/// `#1/31/2024 13:45#`: a date literal when the text up to the next `#` on
/// the line is a date or time.
let private tryLexDate (state: LexerState) =
    let source = state.Source
    let close = source.IndexOf('#', state.Position + 1)
    if close < 0 then None
    else
        let text = source.Substring(state.Position + 1, close - state.Position - 1)
        if text.IndexOf '\n' >= 0 || text.Trim() = "" then None
        else
            match DateCore.tryParse text System.DateTime.Now.Year with
            | Some _ ->
                let mutable st = state
                for _ in state.Position .. close do st <- advance st
                Some (text, st)
            | None -> None

let private lexString (state: LexerState) =
    let stateAfterOpenQuote = advance state
    let startLine = stateAfterOpenQuote.Line
    let startCol = stateAfterOpenQuote.Column + 1
    let chars = System.Text.StringBuilder()
    let mutable st = stateAfterOpenQuote
    let mutable closed = false
    while not closed do
        let c = peek st
        if c = '"' then
            // Check for escaped quote ""
            if peekN st 1 = '"' then
                chars.Append('"') |> ignore
                st <- advance (advance st)
            else
                closed <- true
        elif c = '\000' then
            failwithf "Unterminated string literal starting at line %d, column %d" startLine startCol
        else
            chars.Append(c) |> ignore
            st <- advance st
    let finalState = advance st
    let lexeme = chars.ToString()
    (String lexeme, finalState)

let private lexIdentifier (state: LexerState) =
    let mutable st = state
    while isAlphaNumeric (peek st) do
        st <- advance st
    let lexeme = state.Source.Substring(state.Position, st.Position - state.Position)
    // `Left$`, `name$`, `count%`: the type-declaration character is dropped.
    let st = if (peek st = '$' || peek st = '%') && lexeme <> "_" then advance st else st
    let lower = lexeme.ToLower()
    let isKeyword = Set.contains lower keywords
    // Normalize keyword lexemes to lowercase for consistent pattern matching
    let normalizedLexeme = if isKeyword then lower else lexeme
    (normalizedLexeme, isKeyword, st)

let tokenize (source: string) =
    let initialState = { Source = source; Position = 0; Line = 1; Column = 1 }
    let tokens = ResizeArray<Token>()
    let mutable state = initialState
    let mutable precedingNewline = false
    let mutable afterContinuation = false
    let mutable finished = false
    while not finished do
        let (nextState, hadNewline) = skipWhitespaceTracking state precedingNewline
        state <- nextState
        let nl = hadNewline
        let c = peek state
        let line = state.Line
        let col = state.Column
        let add kind lexeme =
            tokens.Add { Kind = kind; Lexeme = lexeme; Line = line; Column = col; PrecedingNewline = nl }
            afterContinuation <- false

        match c with
        | '\000' -> finished <- true
        | '\'' ->
            state <- skipLineComment (advance state)
            precedingNewline <- true  // comment implies newline
        | '"' ->
            let (lit, newState) = lexString state
            let lexeme = match lit with String s -> s | _ -> ""
            add StringLiteral lexeme
            state <- newState
            precedingNewline <- false
        | c when isDigit c ->
            let (lexeme, newState) = lexNumber state
            add Number lexeme
            state <- newState
            precedingNewline <- false
        | c when isAlpha c ->
            let (lexeme, isKeyword, newState) = lexIdentifier state
            if lexeme = "_" && not isKeyword then
                let hasLeadingSpace =
                    state.Position > 0 && (state.Source.[state.Position - 1] = ' ' || state.Source.[state.Position - 1] = '\t')
                let afterMarker = newState.Position
                let afterCr = if peek newState = '\r' then advance newState else newState
                if not hasLeadingSpace || peek afterCr <> '\n' then
                    failwithf "Invalid line continuation at line %d, column %d" line col
                state <- advance afterCr
                // The physical newline is not a logical statement separator.
                precedingNewline <- nl
                afterContinuation <- true
            else
                let kind = if isKeyword then Ast.Keyword else Ast.Identifier
                add kind lexeme
                state <- newState
                precedingNewline <- false
        | '#' when (match tryLexDate state with Some _ -> true | None -> false) ->
            match tryLexDate state with
            | Some (text, newState) ->
                add DateLiteral text
                state <- newState
            | None -> ()
            precedingNewline <- false
        | '[' ->
            // `[A1]`: everything up to the closing bracket on the same line.
            let mutable s = advance state
            let sb = System.Text.StringBuilder()
            while peek s <> ']' && peek s <> '\n' && peek s <> '\r' && peek s <> '\000' do
                sb.Append(peek s) |> ignore
                s <- advance s
            if peek s <> ']' then failwithf "Expected ']' at line %d, column %d" line col
            add BracketText (sb.ToString())
            state <- advance s
            precedingNewline <- false
        | '(' -> add LParen "("; state <- advance state; precedingNewline <- false
        | ')' -> add RParen ")"; state <- advance state; precedingNewline <- false
        | ',' -> add Comma ","; state <- advance state; precedingNewline <- false
        | ':' -> add Colon ":"; state <- advance state; precedingNewline <- false
        | ';' -> add Semicolon ";"; state <- advance state; precedingNewline <- false
        | '.' ->
            // `Debug.Print .Count` in a With block: a dot after a space starts
            // a With member (" ."), unless the line was continued.
            let spaced =
                state.Position > 0 && (state.Source.[state.Position - 1] = ' ' || state.Source.[state.Position - 1] = '\t')
                && not afterContinuation && not nl
            add Dot (if spaced then " ." else ".")
            state <- advance state
            precedingNewline <- false
        | '=' -> add Eq "="; state <- advance state; precedingNewline <- false
        | '+' -> add Operator "+"; state <- advance state; precedingNewline <- false
        | '-' -> add Operator "-"; state <- advance state; precedingNewline <- false
        | '*' -> add Operator "*"; state <- advance state; precedingNewline <- false
        | '/' -> add Operator "/"; state <- advance state; precedingNewline <- false
        | '\\' -> add Operator "\\"; state <- advance state; precedingNewline <- false
        | '^' -> add Operator "^"; state <- advance state; precedingNewline <- false
        | '<' ->
            let c2 = peekN state 1
            if c2 = '>' then
                add Operator "<>"
                state <- advance (advance state)
            elif c2 = '=' then
                add Operator "<="
                state <- advance (advance state)
            else
                add Operator "<"
                state <- advance state
            precedingNewline <- false
        | '>' ->
            let c2 = peekN state 1
            if c2 = '=' then
                add Operator ">="
                state <- advance (advance state)
            else
                add Operator ">"
                state <- advance state
            precedingNewline <- false
        | '&' ->
            match tryLexRadix state with
            | Some (lexeme, newState) ->
                add Number lexeme
                state <- newState
            | None ->
                add Operator "&"
                state <- advance state
            precedingNewline <- false
        | _ -> add Operator (string c); state <- advance state; precedingNewline <- false
    tokens.Add { Kind = EOF; Lexeme = ""; Line = state.Line; Column = state.Column; PrecedingNewline = precedingNewline }
    resizeArrayToList tokens
