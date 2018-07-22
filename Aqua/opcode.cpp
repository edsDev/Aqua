#include "opcode.h"
#include "ptr-arithmetic.h"

namespace eds::aqua
{
	CodeUnitPtr AdvanceOpCode(CodeUnitPtr ptr, int bytes)
	{
		return reinterpret_cast<CodeUnitPtr>(AdvancePtr(ptr, bytes));
	}

	template<typename T>
	T FetchCodedData(CodeUnitPtr &instRef)
	{
		auto result = *reinterpret_cast<const T*>(instRef);
		AdvanceOpCode(instRef, sizeof(T));

		return result;
	}

	OpCode FetchOpCode(CodeUnitPtr& instRef)
	{
		return FetchCodedData<OpCode>(instRef);
	}
	int8_t FetchArgI8(CodeUnitPtr& instRef)
	{
		return FetchCodedData<int8_t>(instRef);
	}
	int16_t FetchArgI16(CodeUnitPtr& instRef)
	{
		return FetchCodedData<int16_t>(instRef);
	}
	int32_t FetchArgI32(CodeUnitPtr& instRef)
	{
		return FetchCodedData<int32_t>(instRef);
	}
	int64_t FetchArgI64(CodeUnitPtr& instRef)
	{
		return FetchCodedData<int64_t>(instRef);
	}
	float_t FetchArgF32(CodeUnitPtr& instRef)
	{
		return FetchCodedData<float_t>(instRef);
	}
}