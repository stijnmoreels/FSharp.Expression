module FSharp.Expression.Tests

open Swensen.Unquote
open FsCheck.Xunit
open Xunit
open FsCheck
open System
open System.Collections.Generic

let tokenize expression = 
    System.Text.RegularExpressions.Regex.Split(expression, "(AND|OR|\(|\))")
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> List.ofArray

[<Theory>]
[<InlineData("1 = 1 AND 2 != 1", 3)>]
[<InlineData("1 != 2 OR False", 3)>]
[<InlineData("(True OR False) AND (False OR True)", 11)>]
let ``Tokenize in expected count`` expression expected = 
    let actual = tokenize expression
    expected =! List.length actual

let evalBool (expression : string) = 
    match bool.TryParse expression with
    | (false, _) -> None
    | (true, x) -> Some x

[<Theory>]
[<InlineData("True", true)>]
[<InlineData("False", false)>]
let ``Evaluate boolean`` (expressions, expected) = Some expected =! evalBool expressions

let evalLogical (expression : string list) = 
    let stack = new Stack<string>()
    for e in expression do
        stack.Push e
        if stack.Count = 3 then 
            let left = stack.Pop() |> evalBool
            
            let operator = 
                match stack.Pop() with
                | "AND" -> (&&)
                | _ -> (||)
            
            let right = stack.Pop() |> evalBool
            Option.map2 operator left right
            |> Option.get
            |> string
            |> stack.Push
    stack.Pop() |> evalBool

let pop3 list = (List.head list, List.tail list |> List.head, List.last list)

let skipLast number list = 
    List.rev list
    |> List.skip number
    |> List.rev

let evalLogical' (expression : string list) = 
    let rec loop index result = 
        if index = List.length expression then List.head result |> evalBool
        else 
            let temp = result @ [ expression.Item index ]
            if temp.Length = 3 then 
                let left, operator, right = pop3 temp
                
                let operator = 
                    operator |> function 
                    | "AND" -> (&&)
                    | _ -> (||)
                
                let evaluation = 
                    Option.map2 operator (evalBool left) (evalBool right)
                    |> Option.get
                    |> string
                
                loop (index + 1) (skipLast 3 temp @ [ evaluation ])
            else loop (index + 1) temp
    loop 0 []

let evalLogic3 left operator right = 
    let operator = 
        operator |> function 
        | "AND" -> (&&)
        | _ -> (||)
    Option.map2 operator (evalBool left) (evalBool right)
    |> Option.get
    |> string

let evalLogical'' (expression : string list) = 
    let evaluation temp = 
        match temp with
        | left :: operator :: right :: [] -> 
            let withoutLogicalExpression = skipLast 3 temp
            let evaluatedExpression = [ evalLogic3 left operator right ]
            withoutLogicalExpression @ evaluatedExpression
        | _ -> temp
    
    let rec loop index result = 
        match result with
        | x when index = List.length expression -> List.head result |> evalBool
        | _ -> 
            let temp = result @ [ expression.Item index ]
            loop (index + 1) (evaluation temp)
    
    loop 0 []

[<Theory>]
[<InlineData("True AND False", false)>]
[<InlineData("True OR False", true)>]
[<InlineData("False OR True OR False", true)>]
[<InlineData("False OR False AND True OR False", false)>]
let ``Evaluate logical expression`` (expression : string, expected) = 
    let actual = 
        expression.Split ' '
        |> Array.toList
        |> evalLogical''
    Some expected =! actual

let evalEqual (expression : string) = 
    let tupleSides a = (Array.head a, Array.last a)
    let removeEmpties = StringSplitOptions.RemoveEmptyEntries
    let left, right = expression.Split([| ' ' |], removeEmpties) |> tupleSides
    
    let sign = 
        match expression with
        | x when x.Contains "!=" -> (<>)
        | _ -> (=)
    sign left right |> string

[<Theory>]
[<InlineData("1 = 1", "True")>]
[<InlineData("2 != 2", "False")>]
[<InlineData("1 != 2", "True")>]
[<InlineData(" 1 =    1", "True")>]
let ``Evaluate equal expression`` expression expected = 
    let actual = evalEqual expression
    expected =! actual

let evalExp expression = 
    let stack = Stack<string>()
    let tokens = tokenize expression
    
    let evalInParanthesises (stack : Stack<string>) = 
        let rec loop next result = 
            match next with
            | "(" -> result
            | _ -> (next :: result) |> loop (stack.Pop())
        loop (stack.Pop()) []
        |> evalLogical
        |> Option.get
        |> string
    
    let evalToken (t : string) = 
        match t with
        | ")" -> stack |> evalInParanthesises
        | _ when t.Contains "=" -> evalEqual t
        | _ -> t
        |> stack.Push
    
    Seq.iter evalToken tokens
    stack.ToArray()
    |> List.ofArray
    |> evalLogical

let evalInParanthesises result = 
    let inside = List.takeWhile ((<>) "(") result
    let outside = List.rev result |> List.take (result.Length - inside.Length - 1)
    
    let evalInside = 
        evalLogical'' inside
        |> Option.get
        |> string
    List.append outside [ evalInside ]

let evalExp' expression = 
    let tokens = tokenize expression
    
    let rec evalToken index (result : string list) : string list = 
        if index = tokens.Length then result
        else 
            let result = 
                match tokens.Item index with
                | ")" -> evalInParanthesises result
                | t when t.Contains "=" -> evalEqual t :: result
                | t -> t :: result
            evalToken (index + 1) result
    evalToken 0 [] |> evalLogical''

let (|Contains|_|) (input : string) x = 
    if input.Contains x then Some input
    else None

let evalExp'' expression = 
    let tokens = tokenize expression
    
    let evaluation index result = 
        match List.item index tokens with
        | ")" -> evalInParanthesises result
        | t when t.Contains "=" -> evalEqual t :: result
        | t -> t :: result
    
    let rec evalToken index result = 
        match tokens with
        | _ when List.length tokens = index -> result
        | _ -> evalToken (index + 1) (evaluation index result)
    
    evalToken 0 [] |> evalLogical''

[<Theory>]
[<InlineData("1 = 2 AND 3 = 3", false)>]
[<InlineData("1 = 1 AND 2 = 2", true)>]
[<InlineData("(1 = 1 AND 2 = 2) OR 3 != 4", true)>]
[<InlineData("(1 = 1 AND 3 != 4) AND (3 = 1 OR 2 = 6)", false)>]
[<InlineData("(11 != 11 OR (22 = 22 AND 33 = 33) AND 22 != 22) OR 44 = 44", true)>]
let ``Evalute complete expression`` expression expected = 
    let actual = evalExp'' expression
    Some expected =! actual

[<Theory>]
[<InlineData("1 !! = 0")>]
let ``Evaluate None expression`` expression = 
    let actual = evalExp' expression
    None =! actual
