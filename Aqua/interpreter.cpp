#include "interpreter.h"
#include <functional>

namespace eds::aqua::interpret 
{
#pragma region InstructionImpl

	// auxiliary definitions for instruction implementation
	//

	[[noreturn]]
	void ThrowBadOpCode()
	{
		throw 0;
	}

	template<template<typename> typename F, typename TCast, typename T, typename... Ts>
	EvalItem CalcGenericUnaryOperation(const EvalItem& item)
	{
		using ReturnType =
			std::conditional_t<std::is_same_v<TCast, void>, T, TCast>;

		if (item.HoldAlternative<T>())
		{
			auto value = item.GetValueUnchecked<T>();
			auto result = F<T>{}(value);

			return static_cast<ReturnType>(result);
		}
		else if constexpr (sizeof...(Ts) > 0)
		{
			return CalcGenericBinaryOperation<F, Ts...>(lhs, rhs);
		}
		else
		{
			ThrowBadOpCode();
		}
	}

	template<template<typename> typename F, typename TCast, typename... Ts>
	void GenericUnaryInstructionImplAux(EvalContext& ctx)
	{
		// load operands
		auto item = ctx.EvalPop();

		// do type checking
		if (item.OfKlassType())
		{
			ThrowBadOpCode();
		}

		// do calculation and push result back
		ctx.EvalPush(
			CalcGenericUnaryOperation<F, TCast, Ts...>(item));
	}

	template<template<typename> typename F, typename TCast, typename T, typename... Ts>
	EvalItem CalcGenericBinaryOperation(const EvalItem& lhs, const EvalItem& rhs)
	{
		using ReturnType =
			std::conditional_t<std::is_same_v<TCast, void>, T, TCast>;

		if (lhs.HoldAlternative<T>())
		{
			auto lhsValue = lhs.GetValueUnchecked<T>();
			auto rhsValue = rhs.GetValueUnchecked<T>();
			auto result = F<T>{}(lhsValue, rhsValue);

			return static_cast<ReturnType>(result);
		}
		else if constexpr (sizeof...(Ts) > 0)
		{
			return CalcGenericBinaryOperation<F, Ts...>(lhs, rhs);
		}
		else
		{
			ThrowBadOpCode();
		}
	}

	template<template<typename> typename F, typename TCast, typename... Ts>
	void GenericBinaryInstructionImplAux(EvalContext& ctx)
	{
		// load operands
		auto lhs = ctx.EvalPop();
		auto rhs = ctx.EvalPop();

		// do type checking
		if (!lhs.CompareType(rhs) || lhs.OfKlassType())
		{
			ThrowBadOpCode();
		}

		// do calculation and push result back
		ctx.EvalPush(
			CalcGenericBinaryOperation<F, TCast, Ts...>(lhs, rhs));
	}


	template<template<typename> typename F, bool IntegralOnly>
	void GenericUnaryInstructionImpl(EvalContext& ctx)
	{
		if constexpr (IntegralOnly)
		{
			GenericUnaryInstructionImplAux<F, void, int32_t, int64_t>(ctx);
		}
		else
		{
			GenericUnaryInstructionImplAux<F, void, int32_t, int64_t, float_t, double_t>(ctx);
		}
	}

	//
	//
	template<template<typename> typename F, bool IntegralOnly>
	void GenericBinaryInstructionImpl(EvalContext& ctx)
	{
		if constexpr (IntegralOnly)
		{
			GenericBinaryInstructionImplAux<F, void, int32_t, int64_t>(ctx);
		}
		else
		{
			GenericBinaryInstructionImplAux<F, void, int32_t, int64_t, float_t, double_t>(ctx);
		}
	}

	template<template<typename> typename F>
	void GenericComparisonInstructionImpl(EvalContext& ctx)
	{
		GenericBinaryInstructionImplAux<F, int32_t, int32_t, int64_t, float_t, double_t>(ctx);
	}


	template<typename TCast, typename T>
	struct CastHelperImpl
	{
		TCast operator()(T x)
		{
			return static_cast<TCast>(x);
		}
	};

	template<typename TCast>
	struct CastHelperWrapper
	{
		template<typename T>
		using Type = CastHelperImpl<TCast, T>;
	};

	template<typename TFinalStore, typename TCast>
	void GenericCastInstructionImpl(EvalContext& ctx)
	{
		GenericUnaryInstructionImplAux<
			CastHelperWrapper<TCast>::template Type,
			TFinalStore,
			int32_t, int64_t, float_t, double_t>();
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
		auto index = ctx.FetchInstOrData<int32_t>();

		ctx.EvalPush(ctx.ArgumentAt(index));
	}
	void Instruction_LoadLocal(EvalContext& ctx)
	{
		auto index = ctx.FetchInstOrData<int32_t>();

		ctx.EvalPush(ctx.LocalAt(index));
	}
	void Instruction_LoadField(EvalContext& ctx)
	{

	}
	void Instruction_StoreArg(EvalContext& ctx)
	{
		auto index = ctx.FetchInstOrData<int32_t>();
		auto item = ctx.EvalPop();
		auto& slot = ctx.ArgumentAt(index);

		assert(slot.CompareType(item));
		slot = item;
	}
	void Instruction_StoreLocal(EvalContext& ctx)
	{
		auto index = ctx.FetchInstOrData<int32_t>();
		auto item = ctx.EvalPop();
		auto& slot = ctx.LocalAt(index);

		assert(slot.CompareType(item));
		slot = item;
	}
	void Instruction_StoreField(EvalContext& ctx)
	{

	}


	void Instruction_PushI32(EvalContext& ctx)
	{
		ctx.EvalPush(ctx.FetchInstOrData<int32_t>());
	}
	void Instruction_PushF32(EvalContext& ctx)
	{
		ctx.EvalPush(ctx.FetchInstOrData<float_t>());
	}

	void Instruction_Pop(EvalContext& ctx)
	{
		ctx.EvalPop();
	}
	void Instruction_Dup(EvalContext& ctx)
	{
		ctx.EvalDuplicateTop();
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
		GenericBinaryInstructionImpl<ShlHelper, true>(ctx);
	}
	void Instruction_Shr(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<ShrHelper, true>(ctx);
	}
	void Instruction_And(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::bit_and, true>(ctx);
	}
	void Instruction_Or(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::bit_or, true>(ctx);
	}
	void Instruction_Xor(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::bit_xor, true>(ctx);
	}
	void Instruction_Rev(EvalContext& ctx)
	{
		GenericBinaryInstructionImpl<std::bit_not, true>(ctx);
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

	// control flow
	//
	void Instruction_Jump(EvalContext& ctx)
	{
		auto offset = ctx.FetchInstOrData<int32_t>();

		ctx.JumpTo(offset);
	}
	void Instruction_JumpOnTrue(EvalContext& ctx)
	{
		auto offset = ctx.FetchInstOrData<int32_t>();

		if (ctx.EvalPop().GetValue<int32_t>())
		{
			ctx.JumpTo(offset);
		}
	}
	void Instruction_JumpOnFalse(EvalContext& ctx)
	{
		auto offset = ctx.FetchInstOrData<int32_t>();

		if (!ctx.EvalPop().GetValue<int32_t>())
		{
			ctx.JumpTo(offset);
		}
	}
	void Instruction_Ret(EvalContext& ctx)
	{
		ctx.Return();
	}

	// type cast
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
		GenericCastInstructionImpl<int64_t, int64_t>(ctx);
	}

	void Instruction_CastU8(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int32_t, uint8_t>(ctx);
	}
	void Instruction_CastU16(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int32_t, uint16_t>(ctx);
	}
	void Instruction_CastU32(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int32_t, uint32_t>(ctx);
	}
	void Instruction_CastU64(EvalContext& ctx)
	{
		GenericCastInstructionImpl<int64_t, uint64_t>(ctx);
	}

	void Instruction_CastF32(EvalContext& ctx)
	{
		GenericCastInstructionImpl<float_t, float_t>(ctx);
	}
	void Instruction_CastF64(EvalContext& ctx)
	{
		GenericCastInstructionImpl<double_t, double_t>(ctx);
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

	void Interpreter::PerformNextInstruction()
	{
		auto& ctx = CurrentContext();

		switch (ctx.FetchInstOrData<OpCode>())
		{
#define INST_CASE(NAME) \
	case OpCode::NAME: Instruction_##NAME(ctx); break

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

#undef INST_CASE
		}
	}
}