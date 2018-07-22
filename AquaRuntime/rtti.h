#pragma once
#include <memory>
#include "container\heap-array.h"

namespace eds::aqua
{
	struct TypeHandle
	{

	};

	struct KlassDesc;


	struct VarDesc
	{
		int Offset;
		TypeHandle Type;
	};

	struct MethodDesc
	{
		KlassDesc* OwnerKlass;

		int32_t ArgSize;
		int32_t ArgCount;
		std::unique_ptr<VarDesc[]> Args;

		int32_t LocalSize;
		int32_t LocalCount;
		std::unique_ptr<VarDesc[]> Locals;
		
		int32_t ReturnSize;
	};

	struct FieldDesc
	{
		KlassDesc* OwnerKlass;

		TypeHandle Type;
	};

	struct KlassDesc
	{
		const char* Name;

		KlassDesc* Parent;

		MethodDesc* MethodBeginPtr;
		MethodDesc* MethodEndPtr;

		FieldDesc* FieldBeginPtr;
		FieldDesc* FieldEndPtr;
	};
}