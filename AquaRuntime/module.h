#pragma once
#include "mdtoken.h"
#include <memory>

namespace eds::aqua
{
    struct ModuleHeader
    {
        uint32_t TermPadding; // = 'AQUA'

        uint32_t ModuleRefTableOffset;

        uint32_t TypeIndexTableOffset;

        uint32_t TypeDefinitionTableOffset;

        uint32_t MethodIndexTableOffset;

        uint32_t MethodDefinitionTableOffset;

        uint32_t FieldDefinitionTableOffset;

        uint32_t OpcodeBlobOffset;

        uint32_t StringBlobOffset;
    };

    struct ModuleRefItem
    {
        ResToken<res_String> Name;
        ResToken<res_String> Version;
    };

    struct TypeRefItem
    {
        MdToken<md_ModuleRef> SrcModule;
        ResToken<res_String> Name;
    };

    struct MethodRefItem
    {
        MdToken<md_ModuleRef> SrcModule;
        ResToken<res_String> Name;
        ResToken<res_Blob> FuncSig;
    };

    struct FieldRefItem
    {
        MdToken<md_ModuleRef> SrcModule;
        ResToken<res_String> Name;
        ResToken<res_Blob> TypeSig;
    };

    struct TypeSpecItem
    {
        uint32_t Type; // TypeDef or TypeRef
        ResToken<res_Blob> TypeArgs;
    };

    struct MethodSpecItem
    {
        uint32_t Method; // MethodDef or MethodRef
        ResToken<res_Blob> TypeArgs;
    };

    enum class AccessLevel
    {
        Private = 0b00,
        Protected = 0b01,
        Internal = 0b10,
        Public = 0b11,
    };

    struct TypeDefItem
    {
        struct Flags
        {
            AccessLevel Access : 2;
            bool IsVirtual : 1;
            bool IsAbstract : 1;
        };

        ResToken<res_String> Name;
        Flags Flags;
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