#pragma once
#include <memory>

namespace eds::aqua
{
	struct KlassDesc;

	struct MethodTable
	{
        KlassDesc* Klass;

        int32_t VTableSlotCount;
        void* VTableSlots;
	};

	class TypeHandle
	{
    private:
        void* ptr_;
	};

	struct VarDesc
	{
		int Offset;
		TypeHandle Type;
	};

	struct MethodDesc
	{
        MethodTable* MT;

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