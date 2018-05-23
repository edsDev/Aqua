#pragma once
#include "basic.h"
#include "metadata.h"
#include <memory>

namespace eds::aqua
{
	// Assembly:
	//   Types(struct)
	//     Fields
	//   Functions
	//     ReturnType
	//     Parameters
	//     Instructions

	class Module
	{
	private:
		Module() = default;

		static unique_ptr<Module> LoadFromText(zstring data);

	public:
		string_view Name() const
		{
			return name_;
		}

		KlassInfo* LoadType(const string& name) const
		{
			auto iter = type_lookup_.find(name);
			if (iter != type_lookup_.end())
			{
				return iter->second;
			}
			else
			{
				return nullptr;
			}
		}

	private:
		string name_;

		// Class Definition Table
		HeapArray<KlassInfo> klass_list_;

		// Field Definition Table
		// NOTE fields of the same class would always be consecutive
		HeapArray<FieldInfo> field_list_;

		// Method Definition Table
		// NOTE methods of the same class would always be consecutive
		HeapArray<MethodInfo> method_list_;

		unordered_map<string, KlassInfo*> type_lookup_;
	};
}