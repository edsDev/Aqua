#pragma once
#include "basic.h"
#include "opcode.h"
#include <variant>

namespace eds::aqua
{
	enum class PrimaryType
	{
		Unit,
		Boolean,
		Int32,
		Float32,
	};

	struct KlassInfo;
	struct FieldInfo;
	struct MethodInfo;

	struct ParameterInfo;
	struct VariableInfo;

	class TypeStub
	{
	public:
		TypeStub() 
			: TypeStub(PrimaryType::Unit) { }

		TypeStub(PrimaryType kind) 
			: type_data_(kind) { }
		
		TypeStub(const KlassInfo* klass)
			: type_data_(klass) { }

	public:
		bool IsPrimaryType() const
		{

		}

		bool IsKlassType() const
		{

		}

	private:
		std::variant<PrimaryType, const KlassInfo*> type_data_;
	};

	struct TypedName
	{
		TypeStub Type;

		string Name;
	};

	struct ModifierGroup
	{
		bool IsStatic = false;
		bool IsPublic = false;
	};

	struct KlassInfo
	{
		// index of the class in Klass Definition Table
		int32_t Id;

		// name of the class
		string Name;

		// total size of object body
		int32_t Size;

		// methods of the class, view into Method Definition Table
		ArrayView<MethodInfo> Methods;
		// fields of the class, view into Field Definition Table
		ArrayView<FieldInfo> Fields;
	};

	struct FieldInfo
	{
		// index of the field in Field Definition Table
		int32_t Id;

		// name
		string Name;

		// runtime type of the field
		TypeStub Type;

		// object layout offset in byte
		int LayoutOffset;
	};

	struct MethodInfo
	{
		// index of the method in Method Definition Table
		uint32_t Id;

		//
		string Name;

		TypeStub ReturnType;
		HeapArray<TypedName> Parameters;
		HeapArray<TypedName> Locals;

		// unsafe byte sequence
		HeapArray<CodeUnit> Instructions;
	};
}