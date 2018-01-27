#pragma once
#include "basic.h"
#include "metadata.h"

namespace eds::aqua
{
	class Object
	{
	public:
		const TypeInfo* type;
		uint32_t flags;

	public:
		bool ReachableMark() const
		{
			return (flags & 1) != 0;
		}
		void SetReachableMark(bool x)
		{
			flags |= (x ? 1 : 0);
		}

		template <typename T>
		T& Visit(int offset)
		{
			throw 0;
		}
	};
}