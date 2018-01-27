#pragma once
#include <vector>
#include <string>
#include <memory>
#include <variant>
#include <cassert>

namespace eds::aqua::compiler
{
	enum class PrimaryType
	{
		Unit,
		//Object,
		//Byte,
		Int,
		//Float,
		//String,
	};

	enum class VariableMutability
	{
		Val,
		Var,
	};

	class TypeLayoutDef
	{

	};

	class TypeDef
	{
	public:
		std::string_view Name() const
		{
			return std::visit([](const auto& ins) { return ins.Name(); }, ins_);
		}

		bool IsPrimary() const
		{
			return std::visit([](const auto& ins) { return ins.IsPrimary(); }, ins_);
		}
		bool IsDetermined() const
		{
			return std::visit([](const auto& ins) { return ins.IsDetermined(); }, ins_);
		}

		// a placeholder for type to be inferred
		static TypeDef* GetTBD();
		// primary type stub
		static TypeDef* GetPrimary(PrimaryType type);

	private:
		// private ctor, use factory function to create an instance
		TypeDef() = default;

		struct TBDInstance
		{
			std::string_view Name() const { return "TBD"; }
		
			bool IsPrimary() const { return false; }
			bool IsDetermined() const { return false; }
		};

		struct PrimaryInstance
		{
			std::string_view Name() const { return "unit"; }

			bool IsPrimary() const { return true; }
			bool IsDetermined() const { return true; }
		};

		using TypeDefInstance = std::variant<TBDInstance, PrimaryInstance>;
		
		TypeDefInstance ins_;
	};

	class TypeNamePair
	{
	public:
		TypeNamePair(TypeDef* type, const std::string& name)
			: type_(type), name_(name) 
		{
			assert(type != nullptr);
			assert(HasType() || HasName());
		}

		TypeNamePair(const std::string&& name)
			: TypeNamePair(TypeDef::GetTBD(), name) { }
		TypeNamePair(TypeDef* type)
			: TypeNamePair(type, "") { }

		const auto& Type() const { return type_; }
		const auto& Name() const { return name_; }

		bool HasType() const
		{
			return type_->IsDetermined();
		}

		bool HasName() const
		{
			return !name_.empty();
		}

	private:
		TypeDef* type_;
		std::string name_;
	};

	// Type
	//
	
	class Type
	{
	public:
		using Ptr = std::unique_ptr<Type>;
	};

	class BuiltinType : public Type
	{
	private:

	};

	class UserType : public Type
	{

	};

	class FunctionType : public Type
	{

	};

	// Expression
	//

	class Expression
	{
	public:
		using Ptr = std::unique_ptr<Expression>;
		using PtrVec = std::vector<Expression::Ptr>;

	};

	class LiteralExpr : public Expression
	{
	private:
		int value_;
	};

	class VariableExpr : public Expression
	{
	private:
		std::string name_;
	};

	class CompoundExpr : public Expression
	{
	private:
		Expression::PtrVec body_;
	};

	class VariableDeclExpr : public Expression
	{
	private:
		VariableMutability mut_;

		std::string name_;

		Expression::Ptr init_;
	};

	class IfElseExpr : public Expression
	{
	private:
		Expression::Ptr condition_;
		Expression::Ptr positive_body_;
		Expression::Ptr negative_body_;
	};

	class WhileExpr : public Expression
	{
	private:
		Expression::Ptr condition_;
		Expression::Ptr body_;
	};

	class FunctionDef
	{
	private:
		std::string name_;

		std::vector<TypeNamePair> param_types_;

		TypeDef* ret_type_;

		Expression::PtrVec body_;
	};

	class ModuleDef
	{
	private:
		std::vector<FunctionDef> functions_;
	};
}