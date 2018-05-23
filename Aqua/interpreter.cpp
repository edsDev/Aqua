#include "interpreter.h"

namespace eds::aqua::interpret 
{
#pragma region InstructionImpl

	void Instruction_Nop(EvalContext& ctx) { }

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
		auto lhs = ctx.EvalStack.PopItem();
		auto rhs = ctx.EvalStack.PopItem();

		ctx.EvalStack.PushItem(lhs + rhs);
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
				INST_CASE(Box);
				INST_CASE(Unbox);
				INST_CASE(Call);

			default:
				// bad opcode!
				throw 0;
			}
#undef INST_CASE
		}
	}
}