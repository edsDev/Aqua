#pragma once
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

	class Assembly
	{
	public:
		TypeInfo* LoadType(int id);
		FunctionInfo* LoadFunction(int id);

		ArrayView<TypeInfo> Types();
		ArrayView<FunctionInfo> Functions();

	private:
		// store
	};

	std::unique_ptr<Assembly> LoadAssembly(/**/);
}