#pragma once
#include <cstdint>
#include <cassert>

namespace eds::aqua
{
    enum MetaTablePrefix : uint8_t
    {
        md_Any,

        md_ModuleRef,

        md_TypeDef,
        md_TypeRef,
        md_TypeSpec,

        md_MethodDef,
        md_MethodRef,
        md_MethodSpec,

        md_FieldDef,
        md_FieldRef,
    };

    enum ResourcePoolPrefix : uint8_t
    {
        res_String,
        res_Blob,
        res_Bytecode,
    };

    namespace detail
    {
        template <uint32_t Prefix, uint32_t Mask>
        class PrefixedIndex
        {
            static_assert(Prefix & (~Mask) == 0);

        public:
            PrefixedIndex()=default;
            PrefixedIndex(uint32_t index)
                :index_(index)
            {
                assert(index & ValMask == 0);
            }

            uint32_t Index() const { return value & (~Mask); }
            uint32_t Value() const { return value_; }

        private:
            uint32_t value_=0;
        };
    }

    template<MetaTablePrefix Prefix>
    class MdToken : public detail::PrefixedIndex<Prefix << 24, 0xff00'0000>
    {
    public:
        MdToken()=default;
        MdToken(uint32_t index) : PrefixedIndex(index) {}
    };

    template<ResourcePoolPrefix Prefix>
    class ResToken : public detail::PrefixedIndex<Prefix << 24, 0xff00'0000>
    {
    public:
        ResToken()=default;
        ResToken(uint32_t index) : MaskedIndex(index) {}
    };
}
