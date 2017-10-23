#pragma once
#include "basic.h"
#include "assembly.h"
#include "metadata.h"
#include "object.h"
#include "gc.h"
#include <array>
#include <vector>

namespace eds::aqua::interpret
{
	struct EvalItem
	{
		const TypeInfo* type;

		union
		{
			uint8_t dummy[8];

			Object* value_ptr;

			int32_t value_i32;
			// int64_t value_i64;
			// uint32_t value_u32;
			// uint64_t value_u64;
			
			// float value_f32;
			// double value_d32;
		};
	};

	class EvalStack
	{
	public:
		void PushItem(EvalItem item);
		void PushObject(Object* value);
		void PushInt32(int32_t value);

		EvalItem PopItem();

		const auto& Data() const;

	private:
		std::vector<EvalItem> stack_;
	};

	struct EvalContext
	{
		bool returned;
		EvalStack stack;

		OpCode* current_op;
		ArrayView<OpCode> instructions;

		ArrayView<EvalItem> args;
		ArrayView<EvalItem> locals;
	};

	using InstructionDelegate = void (*)(EvalContext& ctx);
	using InstructionDelegateArray = std::array<InstructionDelegate, 256>;

	class Interpreter
	{
	public:

		void Bootstrap()
		{

		}

		void Invoke(FunctionInfo* func)
		{
			static InstructionDelegateArray delegates;

			auto opcodes = func->Instructions();
			auto args = LoadArguments(func);
			auto locals = LoadLocals(func);

			auto p = opcodes.BeginPtr();
			while (true)
			{
				assert(std::distance(p, opcodes.EndPtr()) > 0);
				assert(delegates[p->code.value]);

				// execute and advance opcode at p
				p = delegates[p->code.value](p, opcodes, args, locals);
			}
		}

	private:
		ArrayView<EvalItem> LoadArguments(FunctionInfo* func)
		{
			return TakeEnd(call_stack_, func->Parameters().Length());
		}

		ArrayView<EvalItem> LoadLocals(FunctionInfo* func)
		{

		}

		ArrayView<EvalItem> TakeEnd(std::vector<EvalItem>& v, int len)
		{

		}

		Object* Allocate(TypeInfo* type)
		{
			auto ptr = heap_.Allocate(type);
			
			// return if object is successfully allocated
			if (ptr) return ptr;
			
			// try to collect garbage in the heap and give an attempt again
			CollectGarbage();
			ptr = heap_.Allocate(type);

			// return if object is successfully allocated
			if (ptr) return ptr;

			// allocation failed
			throw 0;
		}

		void CollectGarbage()
		{
			// mark root objects
			//

			for (const auto& item : call_stack_)
			{
				if (item.type->IsKlassType())
					heap_.MarkObject(item.value_ptr);
			}

			// finalize collection
			//
			heap_.CollectGarbage();
		}

		// NOTE caller to recover call site!!!!!!

		gc::ManagedHeap heap_;

		std::vector<EvalItem> call_stack_;
		std::vector<EvalStack*> evals_;
	};
}