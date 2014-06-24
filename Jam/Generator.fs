// Outputs LLVM bytecode for use with the JamVM
module Jam.Generator

open System
open LLVM.Generated.BitWriter
open LLVM.Generated.ExecutionEngine
open LLVM.Generated.Core
open LLVM.Generated.Target
open LLVM.Target
open LLVM.Core
open LLVM.Quote

type JMType =
    | JMNumber
    | JMFuncArg of (string * JMType)

type JMObj<'a> = {
    ty: TypeRef
    value: 'a
}

type JMState = {
    globalVariables: Map<string, ValueRef>;
    scopeVariables: Map<string, ValueRef>;
    localVariables: Map<string, ValueRef>;
    declaredFunctions: Map<string, ValueRef>;
    retVal: ValueRef option;
    builder: Builder;
}

let EmptyState = {
    globalVariables    = Map.empty;
    scopeVariables     = Map.empty;
    localVariables     = Map.empty;
    declaredFunctions  = Map.empty;
    retVal             = None;
    builder            = new Builder();
}

let ExtCallTypes = function
    | "int8" -> int8Type()
    | "int8*" -> pointerType (int8Type()) 64u
    | "int32" -> int32Type()
    | "string" -> pointerType (int8Type()) 64u
    | "void" -> voidType()
    | x -> failwithf "%s is an unknown type." x

type JMId = string

let JMInt = int32Type()

type JMModule = ModuleRef

type JamType = 
    | String of string
    | Number of string

type JamExpr = 
    | JamFunction of (JMId * JamType * JamType list)
    | JamCall of (JMId * JamType list)

let rec llvmTypeFromJM (ty: JMType): TypeRef =
    match ty with
    | JMNumber -> int32Type()
    | JMFuncArg(_, t) -> (llvmTypeFromJM t)

let rec nameFromJM (ty: JMType): string option = 
    match ty with
    | JMFuncArg(m, _) -> Some m
    | _ -> None

let emitLiteralBool (b: bool) =
    if b then constInt (int8Type()) (uint64 0) false
    else constInt (int8Type()) (uint64 1) false

let emitLiteralNumber (value: int) =
    constInt (int32Type()) (uint64 value) false

let emitLiteralString (m: JMModule) (value: string) =
    let v = addGlobal m (arrayType (int8Type()) (uint32 (value.Length + 1))) ".str"
    setGlobalConstant v true
    setExternallyInitialized v false
    setAlignment v 1u
    setLinkage v Linkage.PrivateLinkage

    let strItems = 
        (value.ToCharArray()) 
        |> Array.map (fun (x: char) -> constUInt8 (uint8 x))
        |> Array.rev 
        |> Array.append [|constNull (int8Type())|]
        |> Array.rev
    
    let str = constArray (int8Type()) strItems

    setInitializer v str
    v

let emitExtFuncDeclr (m: JMModule) (name: string) (retTy: string) (argsTy: string list) =
    printfn "Emitting FuncDeclr: %s with %i args" name (argsTy.Length)    
    let fTy = functionType (ExtCallTypes retTy) (argsTy |> List.map (ExtCallTypes) |> Array.ofList)
    let f = addFunction m name fTy
    setLinkage f Linkage.ExternalLinkage
    f

let emitFuncDeclr 
            (m: JMModule) 
            (name: string) 
            (retTy: string) 
            (args: (string * string) list) 
            (body: (Builder -> ValueRef) list) = 

    let fTy = functionType (ExtCallTypes retTy) (args |> List.map(snd) |> List.map (ExtCallTypes) |> Array.ofList)
    let f = addFunction m name fTy

    args
    |> List.map(fst) 
    |> List.iteri (fun i t -> 
        setValueName (getParam f (Convert.ToUInt32(i))) t)

    let entry = appendBasicBlock f "entry"
    use bldr = new Builder()
    positionBuilderAtEnd bldr entry
    body |> List.iter(fun x -> x(bldr) |> ignore)
    f

//    let bldr = createBuilder()
//    // [|(pointerType (int8Type()) 64)|]
//    let s = constString "Hello World" 13u false
//    buildCall bldr f ([|s|]) name // (args |> List.toArray)

let emitFuncCall (name: string) (f: ValueRef) (args: ValueRef list) =
    printfn "Emitting call: %s with %i args" name (args.Length)
    let entry = appendBasicBlock f "fcall"
    use bldr = new Builder()
    positionBuilderAtEnd bldr entry

    buildCall bldr f (args |> List.toArray) name


let createModule (name: string): JMModule =
    moduleCreateWithName name

let dumpModule (m: JMModule) = 
    dumpModule m

let initializeLLVM = 
    initializeX86Target()


//let emitFunction (m: JMModule) 
//                 (functionName: string) 
//                 (functionTy: JMType) 
//                 (functionArgs: JMType list) 
//                 (body: (unit -> ValueRef) list) =
//
//    let funcArgTypes = functionArgs |> List.map(llvmTypeFromJM) |> List.toArray
//    let funcType = functionType (llvmTypeFromJM functionTy) funcArgTypes
//    let func = addFunction m functionName funcType
//
//    functionArgs 
//    |> List.iteri (fun i t -> 
//        nameFromJM t 
//        |> Option.iter(setValueName (getParam func (Convert.ToUInt32(i)))))
//
//    let entry = appendBasicBlock func "entry"
//    use bldr = new Builder()
//    positionBuilderAtEnd bldr entry
//    buildRet bldr (body |> List.map(fun x -> x()) |> List.rev |> List.head) |> ignore
//

