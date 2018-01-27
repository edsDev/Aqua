#pragma once
#include "basic.h"
#include "opcode.h"

namespace eds::aqua
{
	class TypeInfo;
	class FieldInfo;

	class ParameterInfo;
	class VariableInfo;
	class FunctionInfo;

	struct FieldInfo
	{
		// name
		string_view name;
		// data type
		const TypeInfo* type;
		// object layout offset 
		int offset;
	};

	struct TypeInfo
	{
		// unique type id in an assembly
		uint32_t id;

		string_view  name;

		uint32_t flags;
		// size of object body
		int32_t size;
		// view to field information
		ArrayView<FieldInfo> fields;

	public:
		bool IsPrimaryType() const;
		bool IsKlassType() const;
	};

	struct ParameterInfo
	{
		string_view name;

		const TypeInfo* type;
	};

	struct VariableInfo
	{
		string_view name;

		const TypeInfo* type;
	};

	struct FunctionInfo
	{
		// unique function id in an assembly
		uint32_t id;
		//
		string_view name;

		const TypeInfo* return_type;
		ArrayView<ParameterInfo> parameters;
		ArrayView<VariableInfo*> locals;
		ArrayView<OpCode> instructions;
	};
}