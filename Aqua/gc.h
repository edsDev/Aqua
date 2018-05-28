#pragma once
#include "basic.h"
#include "object.h"

namespace eds::aqua::gc
{
	// structure of managed heap:
	// (FreeNode | Object [1 Padding])+
	// a free node have minimal size of 16 bytes

	static constexpr auto kHeapSize = 64 * 1024 * 1024;

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
		ManagedHeap();
		~ManagedHeap();

		ManagedObject* AllocateObject(const KlassInfo* type);

		void MarkObject(ManagedObject* obj);
		void CollectGarbage();

	private:
		void* AllocChunk(int sz);
		void MergeChunk(FreeNode* chunk);

		void* heap_begin;
		void* heap_end;

		FreeNode* free_list_dummy_head;
		FreeNode* free_list_dummy_tail;
	};
}