module Aqua.Bytecode

[<RequireQualifiedAccess>]
type Bytecode =
    | LoadArg of int
    | LoadLocal of int
    //| LoadField
    //| LoadElement
    | StoreArg of int
    | StoreLocal of int
    //| StoreField
    //| StoreElement
    
    // stack operation
    | PushBool of bool
    | PushI32 of int32
    | PushI64 of int64
    | PushU32 of uint32
    | PushU64 of uint64
    | PushF32 of single
    | PushF64 of double
    | Pop
    | Dup

    // arithmetic
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Neg

    // bit operation
    | Shl
    | Shr
    | And
    | Or
    | Xor
    | Rev

    // control flow
    | Jump of int
    | JumpOnTrue of int
    | JumpOnFalse of int
    | Ret
    
    // type cast
    | CastI8
    | CastI16
    | CastI32
    | CastI64
    | CastU8
    | CastU16
    | CastU32
    | CastU64
    | CastF32
    | CastF64
    //| CastObj

    // object model
    //| NewObj
    //| NewArr
    //| Box
    //| Unbox

    | Call of string
    //| CallVirtual

