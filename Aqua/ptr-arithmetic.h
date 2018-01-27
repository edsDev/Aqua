#pragma once
#include <cstdint>

namespace eds::aqua
{
	void* AdvancePointer(void* ptr, int bytes)
	{
		return reinterpret_cast<uint8_t*>(ptr) + bytes;
	}
	const void* AdvancePointer(const void* ptr, int bytes)
	{
		return reinterpret_cast<const uint8_t*>(ptr) + bytes;
	}

	ptrdiff_t PointerDistance(const void* p1, const void* p2)
	{
		return reinterpret_cast<const uint8_t*>(p2) - reinterpret_cast<const uint8_t*>(p1);
	}
}