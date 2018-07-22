#pragma once
#include <memory>

namespace eds::aqua
{
	// [BINARY MODULE LAYOUT]
	// | Header | Dependency Table |
	// | Type Index Table | Type Definition Table |
	// | Method Index Table | Method Definition Table |
	// | Field Definition Table |
	// | Global String Pool |
	//
	// *Header* stores a fixed term "AQUA" and offsets of other components
	// *Dependency Table* stores a list of module reference
	// *Type Index Table" stores indices that point into type definition table
	// *Type Definition Table* stores 
	//
	// [HEADER LAYOUT]
	// - "AQUA"(4 bytes)
	// - Dependency Table Offset(4 bytes)
	// - Type Index Table Offset(4 bytes)
	// - Type Definition Table Offset(4 bytes)
	// - Method Definition Table Offset(4 bytes)
	// - Field Definition Table Offset(4 bytes)
	// - String Blob Offset(4 bytes)

	struct ModuleHeader
	{
		uint32_t TermPadding; // = 'AQUA'

		uint32_t DependencyTableOffset;

		uint32_t TypeIndexTableOffset;

		uint32_t TypeDefinitionTableOffset;

		uint32_t MethodIndexTableOffset;

		uint32_t MethodDefinitionTableOffset;

		uint32_t FieldDefinitionTableOffset;

		uint32_t OpcodeBlobOffset;

		uint32_t StringBlobOffset;
	};

	struct DependencyItem
	{

	};

	struct MethodDefItem
	{
		uint32_t NameOffset;

		uint32_t SourceOffset;

		/* OPCODES */
	};

	// a raw binary module loaded into memory
	class InMemoryModule
	{
	public:

		

	private:
		int m_dataSize;
		std::unique_ptr<const uint8_t[]> m_data;
	};
}