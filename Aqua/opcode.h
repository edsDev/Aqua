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
			not,
			both,
			either,
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

		Code value;
	};

	struct OpCode;
	struct OpCode_1;
	struct OpCode_2;
	struct OpCode_4;

	struct OpCode
	{
		Instruction code;

		OpCode_1* RequireByte();
		OpCode_2* RequireWord();
		OpCode_4* RequireDoubleWord();
	};

	struct OpCode_1
	{
		Instruction code;
		uint8_t operand;
	};
	struct OpCode_2
	{
		Instruction code;
		uint16_t operand;
	};
	struct OpCode_4
	{
		Instruction code;
		uint32_t operand;
	};
}