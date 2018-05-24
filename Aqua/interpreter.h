#pragma once
#include "basic.h"
#include "module.h"
#include "metadata.h"
#include "object.h"
#include "gc.h"
#include <array>
#include <vector>
#include <list>

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

	class EvalItem2
	{
	public:
		template<typename T>
		T GetValue();
	};

	class EvalContext
	{
	public:
		EvalContext(Interpreter& intepreter, const MethodInfo* method);

		// opcode operation
		//
		template<typename T>
		T FetchInstOrData();

		//
		//
		EvalItem& ArgumentAt(int index);
		EvalItem& LocalAt(int index);

		// eval stack operation
		//
		void PushEvalStack(EvalItem item);
		EvalItem PopEvalStack();

		void DuplicateTopEvalStack();

		// control flow
		//
		void JumpTo(int index);

		void Return();
		void Return(EvalItem value);


	private:
		Interpreter& interpreter_;

		vector<EvalItem> evals_;

		const MethodInfo* method_;
		// pointer to next instruction
		CodeUnitPtr inst_;

		HeapArray<EvalItem> args_;
		HeapArray<EvalItem> locals_;
	};

	class InterpreterEnvironment
	{

	};

	class Interpreter
	{
	public:
		Interpreter(unique_ptr<InterpreterEnvironment> env);

		void Bootstrap()
		{
			// invoke main function

			// instruction loop
			while (!context_.empty())
			{
				Tick();
			}
		}

	private:
		void Tick();

		EvalContext& CurrentContext()
		{
			return context_.back();
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
		std::list<EvalContext> context_;
	};
}