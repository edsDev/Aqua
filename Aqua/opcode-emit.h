#pragma once
#include "opcode.h"
#include "binary\binary.h"
#include <vector>
#include <deque>

namespace eds::aqua
{
	class OpCodeGenerator
	{
	private:
		static constexpr int kInvalidLabelOffset = -1;

	public:
		class OffsetLabel
		{
		private:
			friend class OpCodeGenerator;

			int offset_ = kInvalidLabelOffset;
		};

		void Emit(OpCode op)
		{
			EmitInternal(op);
		}
		void Emit(OpCode_1 op)
		{
			EmitInternal(op);
		}
		void Emit(OpCode_2 op)
		{
			EmitInternal(op);
		}
		void Emit(OpCode_4 op)
		{
			EmitInternal(op);
		}

		OffsetLabel* DefineLabel()
		{
			return &labels_.emplace_back();
		}
		void MarkLabel(OffsetLabel* label)
		{
			assert(label->offset_ == kInvalidLabelOffset);
			label->offset_ = writer_.Count();
		}

		std::vector<uint8_t> Export();

	private:
		template<typename T>
		void EmitInternal(const T& op)
		{
			auto begin = reinterpret_cast<const uint8_t*>(op);
			auto end = begin + sizeof(T);

			writer_.Write(begin, end);
		}

		std::deque<OffsetLabel> labels_;
		binary::BinaryWriter writer_;
	};
}