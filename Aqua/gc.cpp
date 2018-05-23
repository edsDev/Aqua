#include "gc.h"
#include "ptr-arithmetic.h"

namespace eds::aqua::gc
{
	FreeNode* MakeFreeNode(void* ptr, int size, FreeNode* prev, FreeNode* next)
	{
		auto result = reinterpret_cast<FreeNode*>(ptr);
		result->dummy = 0;
		result->size = size;
		result->prev = prev;
		result->next = next;

		return result;
	}

	void* InitializeAllocatedChunk(void* ptr, int raw_size, int real_size)
	{
		memset(ptr, 0, raw_size);
		memset(AdvancePointer(ptr, raw_size), 0xffffffff, real_size - raw_size);

		return ptr;
	}

	ManagedHeap::ManagedHeap()
	{
		heap_begin = malloc(kHeapSize);
		heap_end = AdvancePointer(heap_begin, kHeapSize);

		free_list_dummy_head = MakeFreeNode(malloc(sizeof(FreeNode)), 0, nullptr, nullptr);
		free_list_dummy_tail = MakeFreeNode(malloc(sizeof(FreeNode)), 0, nullptr, nullptr);

		auto node = MakeFreeNode(heap_begin, kHeapSize, free_list_dummy_head, free_list_dummy_tail);
		free_list_dummy_head->next = node;
		free_list_dummy_tail->prev = node;
	}

	void* ManagedHeap::AllocChunk(int sz)
	{
		// filter invalid size
		if (sz <= 0 || sz > kHeapSize / 2)
			return nullptr;

		auto raw_size = sz;
		auto real_size = sz;

		// at least 16 bytes is required for one allocation
		if (real_size < 16) real_size = 16;

		// align size to 4
		while (real_size & 0b11) real_size += 1;

		// try to find a chunk of memory to fit
		auto chunk = free_list_dummy_head->next;
		while (chunk != free_list_dummy_tail)
		{
			auto size_delta = chunk->size - real_size;
			if (size_delta < 0)
			{
				// not enough space, check next chunk
				chunk = chunk->next;
			}
			else if (size_delta < 16)
			{
				// additional space won't make another chunk
				// so give away the entire chunk
				real_size = chunk->size;
				chunk->prev->next = chunk->next;
				chunk->next->prev = chunk->prev;

				return InitializeAllocatedChunk(chunk, raw_size, real_size);
			}
			else
			{
				// shrink chunk for allocation
				auto new_chunk = MakeFreeNode(AdvancePointer(chunk, real_size), chunk->size - real_size, chunk->prev, chunk->next);
				chunk->prev->next = new_chunk;
				chunk->next->prev = new_chunk;

				return InitializeAllocatedChunk(chunk, raw_size, real_size);
			}
		}

		// allocation failed
		return nullptr;
	}

	void ManagedHeap::MergeChunk(FreeNode* chunk)
	{
		bool hungry = true;
		while(hungry)
		{
			auto chunk_view = reinterpret_cast<FreeNode*>(AdvancePointer(chunk, chunk->size));
			if (chunk_view->dummy != 0 || chunk_view == heap_end)
			{
				// - given chunk is the last in the heap
				// - next chunk is allocated
				// no merging in both case
				hungry = false;
			}
			else
			{
				// next node is free, remove it from free list and merge it into this chunk
				chunk_view->prev->next = chunk_view->next;
				chunk_view->next->prev = chunk_view->prev;

				chunk->size += chunk_view->size;
			}
		}
	}

	Object* ManagedHeap::AllocateObject(const KlassInfo* type)
	{
		assert(type->IsKlassType());

		auto ptr = reinterpret_cast<Object*>(AllocChunk(type->size + sizeof(Object)));
		ptr->type = type;
		ptr->flags = 0;

		return ptr;
	}

	void ManagedHeap::MarkObject(Object* obj)
	{
		// mark this object
		obj->SetReachableMark(true);

		// mark sub-objects
		auto fields = obj->type->fields;
		for (auto p = fields.BeginPtr(); p != fields.EndPtr(); ++p)
		{
			if (p->type->IsKlassType())
			{
				auto subobj = obj->Visit<Object*>(p->offset);

				if (subobj && !subobj->ReachableMark())
				{
					MarkObject(subobj);
				}
			}
		}
	}

	void ManagedHeap::CollectGarbage()
	{

	}
}