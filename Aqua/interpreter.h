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
	// discriminated union of { monostate_t, int32_t, int64_t, uint32_t, uint32_t, float_t, double_t, Object* }
	class EvalItem
	{
	public:
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

		bool IsKlassType() const;
		bool OfPrimaryType(PrimaryType type) const;

		template<typename T>
		T GetValue() const;

		template<typename T>
		T GetValueUnchecked() const;

	private:

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
		EvalItem& ArgumentAt(int index)
		{
			return args_[index];
		}
		EvalItem& LocalAt(int index)
		{
			return locals_[index];
		}

		// eval stack operation
		//
		void EvalPush(EvalItem item)
		{
			eval_stack_.push_back(item);
		}
		EvalItem EvalPop()
		{
			auto result = eval_stack_.back();
			eval_stack_.pop_back();

			return result;
		}

		void EvalDuplicateTop()
		{
			eval_stack_.push_back(eval_stack_.back());
		}

		// control flow
		//
		void JumpTo(int offset)
		{
			inst_ = AdvanceOpCode(method_->Instructions.begin(), offset);
		}

		void Invoke(const MethodInfo* method)
		{

		}

		void Return()
		{
			interpreter_.PopContext();
		}

	private:
		Interpreter& interpreter_;

		vector<EvalItem> eval_stack_;

		const MethodInfo* method_;
		// pointer to next instruction
		CodeUnitPtr inst_;

		ArrayRef<EvalItem> args_;
		ArrayRef<EvalItem> locals_;
	};

	class InterpreterEnvironment
	{

	};

	class Interpreter
	{
		friend class EvalContext;
	public:
		Interpreter(unique_ptr<InterpreterEnvironment> env);

		void Bootstrap()
		{
			// invoke main function

			// instruction loop
			while (!context_.empty())
			{
				PerformNextInstruction();
			}
		}

	private:
		void PerformNextInstruction();

		EvalContext& CurrentContext()
		{
			return context_.back();
		}

		void PushContext(const MethodInfo* method, ArrayView<EvalItem> args)
		{

		}

		void PopContext()
		{

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