#pragma once
#include <vector>
#include <string>

#include "text/text-utils.hpp"

namespace eds::aqua
{
	using eds::text::zstring;

	template <typename T>
	class ArrayView
	{
	public:
		int Length() const;
		
		const T& At(int index) const;

		T* BeginPtr();
		T* EndPtr();
	};

	using std::string_view;
}