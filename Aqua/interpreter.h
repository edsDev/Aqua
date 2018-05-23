#pragma once
#include "basic.h"
#include "module.h"
#include "metadata.h"
#include "object.h"
#include "gc.h"
#include <array>
#include <vector>

namespace eds::aqua::interpret
{
	struct EvalItem
	{
		TypeStub TypeToken;

		union
		{
			const uint8_t Dummy[8];

			Object* Value_ObjectPtr;

			int32_t Value_I32;
			int64_t Value_I64;
			uint32_t Value_U32;
			uint64_t Value_U64;
			
			float_t Value_F32;
			double_t Value_F64;
		};

	public:
		explicit EvalItem(const KlassInfo* type)
		{
			TypeToken = type;
			Value_ObjectPtr = nullptr;
		}
		EvalItem(Object* ptr)
		{
			TypeToken = ptr->type;
			Value_ObjectPtr = ptr;
		}
		EvalItem(int32_t value)
		{
			TypeToken = PrimaryType::Int32;
			Value_I32 = value;
		}
		EvalItem(float_t value)
		{
			TypeToken = PrimaryType::Float32;
			Value_F32 = value;
		}
	};

	class EvalStack
	{
	public:
		void PushItem(EvalItem item)
		{
			stack_.push_back(item);
		}
		void PushNull(const KlassInfo* type)
		{
			PushItem(EvalItem{ type });
		}
		void PushObject(Object* value)
		{
			PushItem(value);
		}
		void PushInt32(int32_t value)
		{
			PushItem(value);
		}
		void PushFloat32(float_t value)
		{
			PushItem(value);
		}

		void DuplicateTop()
		{
			stack_.push_back(stack_.back());
		}

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
		bool Returned;
		EvalStack EvalStack;

		// pointer to next instruction
		CodeUnitPtr InstPtr;
		ArrayView<CodeUnit> Instructions;

		ArrayRef<EvalItem> Args;
		ArrayRef<EvalItem> Locals;
	};

	class Interpreter
	{
	public:
		Interpreter(unique_ptr<Module> startup);

		void Bootstrap()
		{
			// invoke main function
		}

		void Invoke(const MethodInfo* method);

	private:
		EvalContext CreateEvalContext(const MethodInfo* method)
		{
			EvalContext result;
			
			result.Instructions = method->Instructions.View();
			result.Args = LoadArguments(method);
			result.Locals = LoadLocals(method);

			result.Returned = false;
			result.InstPtr = result.Instructions.BeginPtr();

			return result;
		}

		ArrayRef<EvalItem> LoadArguments(const MethodInfo* method)
		{
			throw 0;
		}

		ArrayRef<EvalItem> LoadLocals(const MethodInfo* method)
		{
			throw 0;
		}

		Object* AllocateObject(const KlassInfo* type)
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
			for (const auto& item : call_stack_)
			{
				if (item.TypeToken.IsKlassType() && item.Value_ObjectPtr)
				{
					heap_.MarkObject(item.Value_ObjectPtr);
				}
			}

			// finalize collection
			heap_.CollectGarbage();
		}

		// NOTE caller to recover call site!!!!!!

		gc::ManagedHeap heap_;

		vector<EvalItem> call_stack_;
		vector<EvalStack*> evals_;
	};
}