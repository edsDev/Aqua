#pragma once
#include "basic.h"
#include "module.h"
#include "metadata.h"
#include "object.h"
#include "gc.h"
#include "ext\type-utils.h"
#include <array>
#include <vector>
#include <list>
#include <variant>

namespace eds::aqua::interpret
{
	class Interpreter;

	// discriminated union of { int32_t, int64_t, float_t, double_t, Object* }
	// by default, int32_t is the type
	class EvalItem
	{
	public:
		EvalItem()
			: value_(static_cast<int32_t>(0)) { }

		EvalItem(ManagedObject* ptr)
			: value_(ptr) { }
		EvalItem(int32_t value)
			: value_(value) { }
		EvalItem(int64_t value)
			: value_(value) { }
		EvalItem(float_t value)
			: value_(value) { }
		EvalItem(double_t value)
			: value_(value) { }

		bool CompareType(const EvalItem& other) const
		{
			return value_.index() == other.value_.index();
		}

		bool OfKlassType() const
		{
			return HoldAlternative<ManagedObject*>();
		}

		ManagedObject* AsObjectPtr() const
		{
			if (OfKlassType())
			{
				return GetValueUnchecked<ManagedObject*>();
			}
			else
			{
				return nullptr;
			}
		}

		template<typename T>
		T HoldAlternative() const
		{
			VerifyTypeParameter<T>();

			return std::holds_alternative<T>(value_);
		}

		template<typename T>
		T GetValue() const
		{
			VerifyTypeParameter<T>();

			if (!std::holds_alternative<T>(value_))
			{
				throw 0;
			}

			return GetValueUnchecked<T>();
		}

		template<typename T>
		T GetValueUnchecked() const
		{
			VerifyTypeParameter<T>();

			return std::get<T>(value_);
		}

	private:
		template<typename T>
		void VerifyTypeParameter() const
		{
			using namespace eds::type;

			constexpr auto condition =
				same_to_any<int32_t, int64_t, uint32_t, uint64_t, float_t, double_t, ManagedObject*>;

			static_assert(Constraint<T>(condition));
		}

		using BackingVariant =
			std::variant<std::monostate, 
						 int32_t, 
						 int64_t, 
						 uint32_t, 
						 uint64_t, 
						 float_t, 
						 double_t, 
						 ManagedObject*>;

		BackingVariant value_;
	};

	class EvalContext
	{
	public:
		EvalContext(Interpreter& intepreter, const MethodInfo* method);

		// opcode operation
		//
		template<typename T>
		T FetchInstOrData()
		{

		}

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

		// evaluation stack for the current method
		vector<EvalItem> eval_stack_;

		// definition info of the current method
		const MethodInfo* method_;

		// pointer to next instruction
		CodeUnitPtr inst_;

		// a modifiable view into arguments in stack
		ArrayRef<EvalItem> args_;

		// a modifiable view into locals in stack
		ArrayRef<EvalItem> locals_;
	};

	class RuntimeEnvironment
	{
		ManagedObject* AllocateObject(const KlassInfo* type)
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
				if (auto ptr = item.AsObjectPtr(); ptr)
				{
					heap_.MarkObject(ptr);
				}
			}

			// finalize collection
			heap_.CollectGarbage();
		}

	private:
		HeapArray<unique_ptr<Module>> loaded_modules_;


		vector<EvalItem> call_stack_;
		std::list<EvalContext> context_;
		gc::ManagedHeap heap_;
	};

	class Interpreter
	{
		friend class EvalContext;
	public:
		Interpreter(unique_ptr<RuntimeEnvironment> env);

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

		void PushContext(const MethodInfo* method, ArrayView<EvalItem> args)
		{

		}

		void PopContext()
		{

		}



		// NOTE caller to recover call site!!!!!!


	};
}