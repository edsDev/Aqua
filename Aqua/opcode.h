#pragma once
#include "basic.h"

namespace eds::aqua
{
	enum class OpCode : uint8_t
	{
		Nop,

		// dynamic stack operation
		//
		LoadArg,
		LoadLocal,
		LoadField,
		//LoadElement,

		StoreArg,
		StoreLocal,
		StoreField,
		//StoreElement,

		// static stack operation
		//
		PushI32,
		//PushI64,
		//PushU32,
		//PushU64,
		PushF32,
		//PushF64,
		//PushStr,
		Pop,
		Dup,

		// arithmetic
		//
		Add,
		Sub,
		Mul,
		Div,
		Rem,
		Neg,

		// bit operation
		//
		Shl,
		Shr,
		And,
		Or,
		Xor,
		Rev,

		// comparison
		//
		Eq,
		NEq,
		Gt,
		GtEq,
		Ls,
		LsEq,

		// control flow
		//
		Jump,
		JumpOnTrue,
		JumpOnFalse,
		Ret,

		// type cast
		CastBool,
		CastI8,
		CastI16,
		CastI32,
		CastI64,
		CastU8,
		CastU16,
		CastU32,
		CastU64,
		CastF32,
		CastF64,
		CastObj,

		// object model
		NewObj,
		//NewArr,
		//Box,
		//Unbox,
		Call
	};
	
	using CodeUnit = uint8_t;
	using CodeUnitPtr = const CodeUnit*;

	CodeUnitPtr AdvanceOpCode(CodeUnitPtr ptr, int bytes);

	OpCode FetchOpCode(CodeUnitPtr& instRef);
	int8_t FetchArgI8(CodeUnitPtr& instRef);
	int16_t FetchArgI16(CodeUnitPtr& instRef);
	int32_t FetchArgI32(CodeUnitPtr& instRef);
	int64_t FetchArgI64(CodeUnitPtr& instRef);
	float_t FetchArgF32(CodeUnitPtr& instRef);
}