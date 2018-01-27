#include "interpreter.h"
#include "ptr-arithmetic.h"
#include <functional>

using namespace std;

namespace eds::aqua::interpret
{
	extern TypeInfo* const PrimaryTypeI32;

	const OpCode* AdvanceOpCode(const OpCode* code, int bytes)
	{
		return reinterpret_cast<const OpCode*>(AdvancePointer(code, bytes));
	}

	const OpCode* FetchOpCode(const OpCode* &code)
	{
		auto result = code;
		code = AdvanceOpCode(code, 1);

		return result;
	}
	const OpCode_1* FetchOpCode_1(const OpCode* &code)
	{
		auto result = reinterpret_cast<const OpCode_1*>(code);
		code = AdvanceOpCode(code, 2);

		return result;
	}
	const OpCode_2* FetchOpCode_2(const OpCode* &code)
	{
		auto result = reinterpret_cast<const OpCode_2*>(code);
		code = AdvanceOpCode(code, 3);

		return result;
	}
	const OpCode_4* FetchOpCode_4(const OpCode* &code)
	{
		auto result = reinterpret_cast<const OpCode_4*>(code);
		code = AdvanceOpCode(code, 5);

		return result;
	}

	void Instruction_Nop(EvalContext& ctx) { }

	void Instruction_LoadConst_I32(EvalContext& ctx) 
	{
		auto op = FetchOpCode_4(ctx.current_op);

		ctx.stack.PushInt32(static_cast<int32_t>(op->operand));
	}
	void Instruction_LoadArg_N(EvalContext& ctx)
	{
		auto op = FetchOpCode_4(ctx.current_op);

		ctx.stack.PushItem(ctx.args.At(op->operand));
	}
	void Instruction_LoadLocal_N(EvalContext& ctx)
	{
		auto op = FetchOpCode_4(ctx.current_op);

		ctx.stack.PushItem(ctx.locals.At(op->operand));
	}

	void Instruction_StoreLocal_N(EvalContext& ctx)
	{
		auto op = FetchOpCode_4(ctx.current_op);
		auto item = ctx.stack.PopItem();
		auto& slot = ctx.locals.At(op->operand);

		assert(slot.type == item.type);
		slot = item;
	}

	template <typename F>
	void GenericUnaryInstruction(EvalContext& ctx)
	{
		auto op = ctx.current_op;
		auto val = ctx.stack.PopItem();

		if (val.type == PrimaryTypeI32)
		{
			ctx.stack.PushInt32(F{}(val.value_i32));
		}
		else
		{
			throw 0;
		}

		ctx.current_op = AdvanceOpCode(op);
	}

	template <typename F>
	void GenericBinaryInstruction(EvalContext& ctx)
	{
		auto op = ctx.current_op;
		auto rhs = ctx.stack.PopItem();
		auto lhs = ctx.stack.PopItem();

		if (lhs.type != rhs.type)
		{
			throw 0;
		}

		if (lhs.type == PrimaryTypeI32)
		{
			ctx.stack.PushInt32(F{}(lhs.value_i32, rhs.value_i32));
		}
		else
		{
			throw 0;
		}

		ctx.current_op = AdvanceOpCode(op);
	}

	InstructionDelegateArray GenerateInstructionDelegateArray()
	{
		InstructionDelegateArray result;

		// Misc
		//
		result[Instruction::nop] = &Instruction_Nop;

		// Load Family
		//
		result[Instruction::ldc_i32] = &Instruction_LoadConst_I32;
		result[Instruction::ldarg_n] = &Instruction_LoadArg_N;
		result[Instruction::ldloc_n] = &Instruction_LoadLocal_N;

		// Store Family
		//
		result[Instruction::stloc_n] = &Instruction_LoadLocal_N;

		// Arithmetic
		//
		result[Instruction::add] = &GenericBinaryInstruction<plus<>>;
		result[Instruction::sub] = &GenericBinaryInstruction<minus<>>;
		result[Instruction::mul] = &GenericBinaryInstruction<multiplies<>>;
		result[Instruction::div] = &GenericBinaryInstruction<divides<>>;
		result[Instruction::mod] = &GenericBinaryInstruction<modulus<>>;
		result[Instruction::neg] = &GenericUnaryInstruction<negate<>>;

		// Bitwise
		//
		result[Instruction::and] = &GenericBinaryInstruction<bit_and<>>;
		result[Instruction::or] = &GenericBinaryInstruction<bit_or<>>;
		result[Instruction::xor] = &GenericBinaryInstruction<bit_xor<>>;
		result[Instruction::rev] = &GenericUnaryInstruction<bit_not<>>;

		// Logical
		//
		result[Instruction::not] = &GenericUnaryInstruction<logical_not<>>;
		result[Instruction::cmp_eq] = &GenericBinaryInstruction<equal_to<>>;
		result[Instruction::cmp_neq] = &GenericBinaryInstruction<not_equal_to<>>;
		result[Instruction::cmp_lt] = &GenericBinaryInstruction<less<>>;
		result[Instruction::cmp_lteq] = &GenericBinaryInstruction<less_equal<>>;
		result[Instruction::cmp_gt] = &GenericBinaryInstruction<greater<>>;
		result[Instruction::cmp_gteq] = &GenericBinaryInstruction<greater_equal<>>;

		return result;
	}
}