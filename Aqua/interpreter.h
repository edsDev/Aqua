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
		void PushItem(EvalItem item)
		{
			stack_.push_back(item);
		}
		void PushNull(TypeInfo* type);
		void PushObject(Object* value);
		void PushInt32(int32_t value);

		EvalItem PopItem()
		{
			auto result = stack_.back();
			stack_.pop_back();

			return result;
		}

		const auto& Data() const
		{
			return stack_;
		}

	private:
		std::vector<EvalItem> stack_;
	};

	struct EvalContext
	{
		bool returned;
		EvalStack stack;

		const OpCode* current_op;
		ArrayView<OpCode> instructions;

		ArrayView<EvalItem> args;
		ArrayRef<EvalItem> locals;
	};

	using InstructionDelegate = void (*)(EvalContext& ctx);
	using InstructionDelegateArray = std::array<InstructionDelegate, 256>;

	class Interpreter
	{
	public:
		Interpreter(std::unique_ptr<Assembly> assembly);

		void Bootstrap()
		{
			// invoke main function
		}

		void Invoke(FunctionInfo* func)
		{
			static InstructionDelegateArray delegates;

			auto ctx = CreateEvalContext(func);
			while (!ctx.returned)
			{
				assert(std::distance(ctx.current_op, ctx.instructions.EndPtr()) > 0);
				assert(delegates[ctx.current_op->code]);

				// execute and advance opcode at p
				delegates[ctx.current_op->code](ctx);
			}
		}

	private:
		EvalContext CreateEvalContext(FunctionInfo* func)
		{
			EvalContext result;
			
			result.instructions = func->instructions;
			result.args = LoadArguments(func);
			result.locals = LoadLocals(func);

			result.returned = false;
			result.current_op = result.instructions.BeginPtr();

			return result;
		}

		ArrayView<EvalItem> LoadArguments(FunctionInfo* func)
		{
			throw 0;
		}

		ArrayRef<EvalItem> LoadLocals(FunctionInfo* func)
		{
			throw 0;
		}

		Object* AllocateObject(TypeInfo* type)
		{
			auto ptr = heap_.AllocateObject(type);
			
			// return if object is successfully allocated
			if (ptr) return ptr;
			
			// try to collect garbage in the heap and give an attempt again
			CollectGarbage();
			ptr = heap_.AllocateObject(type);

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