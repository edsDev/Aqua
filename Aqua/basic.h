#pragma once

#include "container\heap-array.h"
#include "array-ref.h"
#include "text/text-utils.h"

#include <string>
#include <memory>
#include <vector>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

namespace eds::aqua
{
	using eds::text::zstring;
	using eds::container::HeapArray;

	using std::string;
	using std::string_view;

	using std::unique_ptr;
	using std::shared_ptr;

	using std::vector;
	using std::map;
	using std::set;
	using std::unordered_map;
	using std::unordered_set;
}