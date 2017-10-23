#pragma once
#include "basic.h"
#include "object.h"

namespace eds::aqua::gc
{
	struct FreeNode
	{
		int dummy;
		int size; // from *this to end of free space
		FreeNode* prev;
		FreeNode* next;

		/* bytes */
	};

	class ManagedHeap
	{
	public:
		Object* Allocate(TypeInfo* type);

		void MarkObject(Object* obj);
		void CollectGarbage();
	};
}