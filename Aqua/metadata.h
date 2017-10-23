#pragma once
#include "basic.h"
#include "opcode.h"

namespace eds::aqua
{
	class FieldInfo
	{
	public:
		TypeInfo* Type() const;
		string_view Name() const;
	};

	class TypeInfo
	{
	public:
		string_view Name() const;

		bool IsPrimaryType() const;
		bool IsEnumType() const;
		bool IsKlassType() const;

		int Size() const;
		ArrayView<FieldInfo*> Fields();
	};

	class ParameterInfo
	{
	public:
		TypeInfo* Type() const;
		string_view Name() const;
	};

	class VariableInfo
	{
	public:
		TypeInfo* Type() const;
		string_view Name() const;
	};

	class FunctionInfo
	{
	public:
		string_view Name()	const;

		TypeInfo* ReturnType() const;
		ArrayView<ParameterInfo*> Parameters() const;

		ArrayView<VariableInfo*> LocalVariables() const;
		ArrayView<OpCode> Instructions() const;
	};
}