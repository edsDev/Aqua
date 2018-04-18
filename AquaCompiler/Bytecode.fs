module Aqua.Bytecode

type Bytecode =
    | Nop

    | LoadArg of int
    | LoadLocal of int
    //| LoadField
    //| LoadElement
    | StoreArg of int
    | StoreLocal of int
    //| StoreField
    //| StoreElement

    // stack operation
    | PushI32 of int32
    | PushI64 of int64
    | PushU32 of uint32
    | PushU64 of uint64
    | PushF32 of single
    | PushF64 of double
    | PushStr of string
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

    // comparison
    | Eq

    // control flow
    | Jump of int
    | JumpOnTrue of int
    | JumpOnFalse of int
    | Ret

    // type cast
    | CastBool
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
    | CastObj of string

    // object model
    //| NewObj
    //| NewArr
    //| Box
    //| Unbox

    | Call of string
    //| CallVirtual

type BytecodeAccumulator = ResizeArray<Bytecode>

module CodeGen =
    let createEmpty () =
        BytecodeAccumulator()

    let extractNextIndex (acc: BytecodeAccumulator) =
        acc.Count

    let appendBytecode (acc: BytecodeAccumulator) code =
        acc.Add(code)

    let appendDummy (acc: BytecodeAccumulator) =
        let index = extractNextIndex acc

        acc.Add(Nop)
        fun code -> acc.[index] <- code

