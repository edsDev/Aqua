#pragma once
#include "basic.h"

namespace eds::aqua
{
	struct Instruction
	{
		enum Code : uint8_t
		{
			nop,

			// Load Family
			//
			ldc_i32,
			ldarg_n,
			ldloc_n,
			// ldfld,

			// Store Family
			//
			stloc_n,
			// stfld,

			// Type Cast Family
			//

			// Arithmetic Operation Family
			//
			add,
			sub,
			mul,
			div,
			mod,
			neg,

			// Bitwise Operation Family
			//
			and,
			or,
			xor,
			rev,
			// shl,
			// shr,
			// shr_u,

			// Logical Operation Family
			//
			lg_not,
			lg_and,
			lg_or,
			cmp_eq,
			cmp_neq,
			cmp_lt,
			cmp_lteq,
			cmp_gt,
			cmp_gteq,

			// Control Flow Family
			//
			jmp,
			jmpif,
			call,
			ret,

			// Dynamic Allocation Family
			//
			// newobj,
			// newarr,
		};
	};

	struct OpCode;
	struct OpCode_1;
	struct OpCode_2;
	struct OpCode_4;

#pragma pack(push, 1)
	struct OpCode
	{
		Instruction::Code code;
	};

	struct OpCode_1
	{
		Instruction::Code code;
		uint8_t operand;
	};
	struct OpCode_2
	{
		Instruction::Code code;
		uint16_t operand;
	};
	struct OpCode_4
	{
		Instruction::Code code;
		uint32_t operand;
	};
#pragma pack(pop)

}