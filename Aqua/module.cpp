#include "module.h"

using namespace std;
using namespace eds::text;

namespace eds::aqua 
{
	[[noreturn]]
	void ThrowParsingError()
	{
		throw 0;
	}

	void SkipWhitespace(zstring& s)
	{
		while (ConsumeIfAny(s, " \t\r\n")) { }
	}

	bool TrySkipKeyword(zstring& s, zstring data)
	{
		SkipWhitespace(s);
		return ConsumeIfSeq(s, data);
	}

	void SkipKeyword(zstring& s, zstring data)
	{
		if (!TrySkipKeyword(s, data))
		{
			ThrowParsingError();
		}
	}

	string ParseIdentifier(zstring& s)
	{
		SkipWhitespace(s);

		string buf;
		for (auto ch = *s; isalnum(*s) || *s == '$' || *s == '@'; ++s)
		{
			buf.push_back(*s);
		}

		if (buf.empty())
		{
			ThrowParsingError();
		}

		return buf;
	}

	string ParseModuleName(zstring& s)
	{
		SkipKeyword(s, ".module");
		return ParseIdentifier(s);
	}

	ModifierGroup ParseModifierGroup(zstring& s)
	{
		ModifierGroup result;

		SkipKeyword(s, "[");

		SkipKeyword(s, "]");
	}

	struct RawKlassData
	{
		
	};

	HeapArray<KlassInfo> ParseKlassList(zstring& s)
	{
		vector<unique_ptr<KlassInfo>> buffer;
		while (TrySkipKeyword(s, ".class"))
		{
			auto modifiers = ParseModifierGroup(s);
			SkipKeyword(s, "{");

			SkipKeyword(s, "}");
		}
	}

	unique_ptr<Module> Module::LoadFromText(zstring text)
	{
		auto result = unique_ptr<Module>{ new Module };
		result->name_ = ParseModuleName(text);
		result->klass_list_ = ParseKlassList(text);

		for (const auto& info : result->klass_list_)
		{
			result->type_lookup_.insert_or_assign(info.name, &info);
		}
	}
}