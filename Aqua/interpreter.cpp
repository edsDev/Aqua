#include "interpreter.h"
#include <functional>

namespace eds::aqua::interpret 
{
#pragma region InstructionImpl

	// auxiliary definitions for instruction implementation
	//

	template<template<typename> typename F, bool IntegralOnly>
	void GenericUnaryInstructionImpl(EvalContext& ctx)
	{

	}

	template<template<typename> typename F, bool IntegralOnly>
	void GenericBinaryInstructionImpl(EvalContext& ctx)
	{
		auto lhs = ctx.EvalStack.PopItem();
		auto rhs = ctx.EvalStack.PopItem();

		if (lhs.TypeToken != rhs.TypeToken)
		{
			throw 0;
		}
		if (lhs.TypeToken.IsKlassType())
		{
			throw 0;
		}

		// integral
		//
		if (lhs.TypeToken == PrimaryType::Int32)
		{
			ctx.EvalStack.PushInt32(F<int32_t>{}(lhs.Value_I32, rhs.Value_I32));
			return;
		}

		// float
		//
		if constexpr(!IntegralOnly)
		{
			if (lhs.TypeToken == PrimaryType::Float32)
			{
				ctx.EvalStack.PushFloat32(F<float_t>{}(lhs.Value_F32, rhs.Value_F32));
			}
		}

		// bad opcode
		throw 0;
	}

	template<template<typename> typename F>
	void GenericComparisonInstructionImpl(EvalContext& ctx)
	{

	}

	template<typename TStore, typename TCast>
	void GenericCastInstructionImpl(EvalContext& ctx)
	{

	}

	template<typename T>
	struct ShlHelper
	{
		T operator()(T x, T y)
		{
			return x << y;
		}
	};
	template<typename T>
	struct ShrHelper
	{
		T operator()(T x, T y)
		{
			return x >> y;
		}
	};

	// nop
	//

	void Instruction_Nop(EvalContext& ctx) { }

	// dynamic stack operation
	//

	void Instruction_LoadArg(EvalContext& ctx)
	{
		auto index = FetchArgI32(ctx.InstPtr);

		ctx.EvalStack.PushItem(ctx.Args.At(index));
	}
	void Instruction_LoadLocal(EvalContext& ctx)
	{
		auto index = FetchArgI32(ctx.InstPtr);

		ctx.EvalStack.PushItem(ctx.Locals.At(index));
	}
	void Instruction_LoadField(EvalContext& ctx)
	{

	}
	void Instruction_StoreArg(EvalContext& ctx)
	{
		auto index = FetchArgI32(ctx.InstPtr);
		auto item = ctx.EvalStack.PopItem();
		auto& slot = ctx.Args.At(index);

		assert(slot.type == item.type);
		slot = item;
	}
	void Instruction_StoreLocal(EvalContext& ctx)
	{
		auto index = FetchArgI32(ctx.InstPtr);
		auto item = ctx.EvalStack.PopItem();
		auto& slot = ctx.Locals.At(index);

		assert(slot.type == item.type);
		slot = item;
	}
	void Instruction_StoreField(EvalContext& ctx)
	{

	}


	void Instruction_PushI32(EvalContext& ctx)
	{
		ctx.EvalStack.PushInt32(FetchArgI32(ctx.InstPtr));
	}
	void Instruction_PushF32(EvalContext& ctx)
	{
		ctx.EvalStack.PushFloat32(FetchArgF32(ctx.InstPtr));
	}

	void Instruction_Pop(EvalContext& ctx)
	{
		ctx.EvalStack.PopItem();
	}
	void Instruction_Dup(EvalContext& ctx)
	{
		ctx.EvalStack.DuplicateTop();
	}

	// arithmetic
	//

	void Instruction_Add(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::plus, false>(ctx);
	}
	void Instruction_Sub(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::minus, false>(ctx);
	}
	void Instruction_Mul(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::multiplies, false>(ctx);
	}
	void Instruction_Div(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::divides, false>(ctx);
	}
	void Instruction_Rem(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::modulus, true>(ctx);
	}
	void Instruction_Neg(EvalContext& ctx)
	{
		GenericUnaryInstructionImpl<std::negate, false>(ctx);
	}

	// bit operation
	//

	void Instruction_Shl(EvalContext& ctx)
	{
		GenericUnaryInstructionImpl<ShlHelper, true>(ctx);
	}
	void Instruction_Shr(EvalContext& ctx)
	{
		GenericUnaryInstructionImpl<ShrHelper, true>(ctx);
	}
	void Instruction_And(EvalContext& ctx)
	{
		GenericUnaryInstructionImpl<std::bit_and, true>(ctx);
	}
	void Instruction_Or(EvalContext& ctx)
	{
		GenericUnaryInstructionImpl<std::bit_or, true>(ctx);
	}
	void Instruction_Xor(EvalContext& ctx)
	{
		GenericUnaryInstructionImpl<std::bit_xor, true>(ctx);
	}
	void Instruction_Rev(EvalContext& ctx)
	{
		GenericUnaryInstructionImpl<std::bit_not, true>(ctx);
	}

	// comparison
	//
	void Instruction_Eq(EvalContext& ctx)
	{
		GenericComparisonInstructionImpl<std::equal_to>(ctx);
	}
	void Instruction_NEq(EvalContext& ctx)
	{
		GenericComparisonInstructionImpl<std::not_equal_to>(ctx);
	}
	void Instruction_Gt(EvalContext& ctx)
	{
		GenericComparisonInstructionImpl<std::greater>(ctx);
	}
	void Instruction_GtEq(EvalContext& ctx)
	{
		GenericComparisonInstructionImpl<std::greater_equal>(ctx);
	}
	void Instruction_Ls(EvalContext& ctx)
	{
		GenericComparisonInstructionImpl<std::less>(ctx);
	}
	void Instruction_LsEq(EvalContext& ctx)
	{
		GenericComparisonInstructionImpl<std::less_equal>(ctx);
	}

	// 
	//
	void Instruction_Jump(EvalContext& ctx)
	{
		auto offset = FetchArgI32(ctx.InstPtr);

		ctx.InstPtr = AdvanceOpCode(ctx.Instructions.BeginPtr(), offset);
	}
	void Instruction_JumpOnTrue(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_JumpOnFalse(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_Ret(EvalContext& ctx)
	{
		throw 0;
	}

	//
	//
	void Instruction_CastBool(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int32_t, bool>(ctx);
	}

	void Instruction_CastI8(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int32_t, int8_t>(ctx);
	}
	void Instruction_CastI16(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int32_t, int16_t>(ctx);
	}
	void Instruction_CastI32(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int32_t, int32_t>(ctx);
	}
	void Instruction_CastI64(EvalContext& ctx)
	{
		throw 0;
	}

	void Instruction_CastU8(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_CastU16(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_CastU32(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_CastU64(EvalContext& ctx)
	{
		throw 0;
	}

	void Instruction_CastF32(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_CastF64(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_CastObj(EvalContext& ctx)
	{
		throw 0;
	}

	// object model
	//
	void Instruction_NewObj(EvalContext& ctx)
	{
		throw 0;
	}
	void Instruction_Call(EvalContext& ctx)
	{
		throw 0;
	}


#pragma endregion

	void Interpreter::Invoke(const MethodInfo* method)
	{
		auto ctx = CreateEvalContext(method);
		while (!ctx.Returned)
		{
#define INST_CASE(NAME) \
	case OpCode::NAME: Instruction_##NAME(ctx); break

			switch (FetchOpCode(ctx.InstPtr))
			{
				// nop
				//
				INST_CASE(Nop);

				// dynamic stack operation
				//
				INST_CASE(LoadArg);
				INST_CASE(LoadLocal);
				INST_CASE(LoadField);

				INST_CASE(StoreArg);
				INST_CASE(StoreLocal);
				INST_CASE(StoreField);

				// static stack operation
				//
				INST_CASE(PushI32);
				INST_CASE(PushF32);
				INST_CASE(Pop);
				INST_CASE(Dup);

				// arithmetic
				//
				INST_CASE(Add);
				INST_CASE(Sub);
				INST_CASE(Mul);
				INST_CASE(Div);
				INST_CASE(Rem);
				INST_CASE(Neg);

				// bit operation
				//
				INST_CASE(Shl);
				INST_CASE(Shr);
				INST_CASE(And);
				INST_CASE(Or);
				INST_CASE(Xor);
				INST_CASE(Rev);

				// comparison
				//
				INST_CASE(Eq);
				INST_CASE(NEq);
				INST_CASE(Gt);
				INST_CASE(GtEq);
				INST_CASE(Ls);
				INST_CASE(LsEq);

				// control flow
				//
				INST_CASE(Jump);
				INST_CASE(JumpOnTrue);
				INST_CASE(JumpOnFalse);
				INST_CASE(Ret);
				
				// type cast
				//
				INST_CASE(CastBool);

				INST_CASE(CastI8);
				INST_CASE(CastI16);
				INST_CASE(CastI32);
				INST_CASE(CastI64);

				INST_CASE(CastU8);
				INST_CASE(CastU16);
				INST_CASE(CastU32);
				INST_CASE(CastU64);

				INST_CASE(CastF32);
				INST_CASE(CastF64);

				INST_CASE(CastObj);

				// object model
				//
				INST_CASE(NewObj);
				//INST_CASE(Box);
				//INST_CASE(Unbox);
				INST_CASE(Call);

			default:
				// bad opcode!
				throw 0;
			}
#undef INST_CASE
		}
	}
}