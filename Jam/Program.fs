module Jam.Main
open System
open System.IO

open FParsec
open Generator

/// Stream debugger.
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

type Expr =
    | Atom of string
    | TypedAtom of string * string
    | List of Expr list
    | DottedList of Expr list * Expr
    | Number of int
    | String of string
    | Bool of bool

type PExpr = Parser<Expr, unit>

let lp : Parser<char, unit> = pchar '('
let rp : Parser<char, unit> = pchar ')'
let speech : Parser<char, unit> = pchar '"'
let dot : Parser<char, unit> = pchar '.'
let quote : Parser<char, unit> = pchar '\''

let symbol = anyOf "#!$%&|*+-/<=>?@^_~"

let charListToString = List.fold (sprintf "%s%c") ""

let parseString : PExpr = 
    speech >>.
    many (noneOf "\"") .>> 
    speech >>= fun x ->
        preturn (String (charListToString x))

let parseTypedAtom : PExpr =
    (letter <|> symbol) .>>. (many (letter <|> digit <|> symbol)) .>> pchar ':' .>>. (many (letter <|> digit)) |>> fun atom ->
        let fs = (fst atom)
        let id = (fst fs) :: (snd fs) |> charListToString
        let typ = (snd atom) |> charListToString
        TypedAtom(id, typ)

let parseAtom : PExpr =
    (letter <|> symbol) .>>. (many (letter <|> digit <|> symbol)) |>> fun atom ->
        let atomStr = (fst atom) :: (snd atom) |> charListToString
        match atomStr with
            | "#t" -> Bool true
            | "#f" -> Bool false
            | x -> Atom x

let parseExpr, parseExprImpl = createParserForwardedToRef()
let parseExprList = many1 (spaces >>. parseExpr .>> spaces) |>> List
let parseNumber : PExpr = many1 digit |>> (charListToString >> (Int32.Parse) >> Number)
let parseList : PExpr = (sepBy1 parseExpr spaces1) |>> List
let parseDottedList : PExpr = (sepEndBy1 parseExpr spaces1) .>> dot .>> spaces1 .>>. parseExpr |>> DottedList 
let parseQuoted : PExpr = quote >>. parseExpr |>> fun x -> List([Atom "quote"; x])

do
    parseExprImpl := (attempt parseTypedAtom) <!> "parsing typed atom"
                 <|> parseAtom <!> "parsing atom"
                 <|> parseString <!> "parsing string"
                 <|> parseNumber <!> "parsing number"
                 <|> parseQuoted <!> "parsing quoted"
                 <|> (lp >>. (attempt (parseList <!> "parsing list") <|> (parseDottedList <!> "parsing dotted list")) .>> rp)

let rec showVal = function
    | (String x)   -> x
    | (Atom name)  -> name
    | (TypedAtom (name, typ)) -> sprintf "%s:%s" name typ
    | (Number num) -> num.ToString()
    | (Bool true)  -> "#t"
    | (Bool false) -> "#f"
    | (List xs)    -> sprintf "(%s)" (xs |> List.fold (fun s x -> sprintf "%s%s " s (showVal x)) "")
    | (DottedList (head, tail)) -> sprintf "(%s . %s)" (head |> List.fold (fun s x -> sprintf "%s%s " s (showVal x)) "") (showVal tail)

let unquoteList = function
    | (List [Atom "quote"; (List x)]) -> x
    | _ -> failwith "item is not a list"

let typedAtomToTuple = function
    | TypedAtom (name, typ) -> (name, typ)
    | x -> failwithf "unable to extract type information from non-typed node (%A)" x

let extractName = function
    | TypedAtom (name, _) -> name
    | x -> failwithf "unable to extract name from non-typed node (%A)" x

let extractType = function
    | TypedAtom (_, typ) -> typ
    | x -> failwithf "unable to extract type from non-typed node (%A)" x

let rec emit m value (state: Generator.JMState) =
    //printfn "Compiler State: %A" state 
    match value with    
    | (String x) -> 
        { state with
            retVal = Some (Generator.emitLiteralString m x) }
    | (Number x) -> 
        { state with
            retVal = Some (Generator.emitLiteralNumber x) }
    | (Bool x) -> 
        { state with
            retVal = Some (Generator.emitLiteralBool x) }
    | (List ([Atom "declare"; name; rettyp; List (args)])) -> 
        let decl = (Generator.emitExtFuncDeclr m (showVal name) (showVal rettyp) (args |> List.map showVal))
        { state with 
            declaredFunctions = state.declaredFunctions.Add(showVal name, decl)
            retVal = Some decl }

    | (List ([Atom "def"; TypedAtom (name, ftype); List args; body])) -> 
        let argTyps = (args |> List.map(typedAtomToTuple))

        emit m body state
        //body |> List.fold(fun s x -> emit m x s) state

        let f = (Generator.emitFuncDeclr m name ftype argTyps [])
        { state with
            globalVariables = state.globalVariables.Add(name, f)
            retVal = Some f }
    | (List ((Atom f)::args)) ->
        if (state.declaredFunctions.ContainsKey f) then
            let fargs = 
                args 
                |> List.map(fun x -> emit m x state) 
                |> List.map(fun x -> x.retVal)
                |> List.choose(fun x -> x)

            let c = Generator.emitFuncCall f (state.declaredFunctions.Item f) fargs
            { state with
                retVal = Some c }
        else
            failwithf "Function not found in scope: %s" f
        
        //let c = Generator.emit

    | x -> failwithf "%A is unmatched" x
           
let prettyPrintArgs xs = 
   xs |> List.tail |> List.fold (sprintf "%s, %s") (xs |> List.head)
        
let readExpr (inputFile: string): Expr =
    match (runParserOnFile (parseExprList) () inputFile (Text.Encoding.UTF8)) with
    | Success (s, _, _) -> s
    | Failure (e, _, _) -> failwithf "No match: %A" e

[<EntryPoint>]
let main args =
    Generator.initializeLLVM
    let m = Generator.createModule "bob"
    let tree = readExpr args.[0]
    //printfn "%A" tree

    match tree with 
    | (List xs) -> 
        xs |> List.fold(fun s x -> emit m x s) (Generator.EmptyState) |> ignore
    | x -> emit m x (Generator.EmptyState) |> ignore
    
    Generator.dumpModule m |> printfn "%A"

//    Generator.emitNumber m "test1" 10
//    Generator.emitNumber m "test2" 20
   // Generator.emitLiteralString m "Hello World!"
    //Generator.emitFunction m "woo"
//    Generator.dumpModule m |> printfn "%A"


//    let c = File.ReadAllText(args.[0])
//    printfn "%A" (readExpr c)
    0
