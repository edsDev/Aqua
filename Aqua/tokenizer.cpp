#include "tokenizer.h"
#include <cassert>

using namespace std;

namespace eds::aqua::compiler
{
	Token LoadToken(const char* begin, const char* end)
	{
		assert(begin && end && distance(begin, end) >= 0);

		// skip whitespace
		while (begin != end && isspace(*begin))
		{
			++begin;
		}

		// identifier eof
		if (begin == end)
		{
			return Token{ TokenCategory::EndOfFile, { end, 0 } };
		}

		// try lex meta-symbols
		switch (*begin)
		{
		case ',':
			return Token{ TokenCategory::Comma,{ begin, 1 } };
		case '(':
			return Token{ TokenCategory::OpenParathe,{ begin, 1 } };
		case ')':
			return Token{ TokenCategory::CloseParathe,{ begin, 1 } };
		case '[':
			return Token{ TokenCategory::OpenBracket,{ begin, 1 } };
		case ']':
			return Token{ TokenCategory::CloseBracket,{ begin, 1 } };
		}

		// try lex identifier
		if (isalpha(*begin))
		{
			auto p = begin;
			while (isalnum(*(++p))) { }

			return Token{ TokenCategory::Identifier, { begin, distance(begin, p) } };
		}


		// try lex literals
		if (isdigit(*begin))
		{
			auto p = begin;
			while (isdigit(*(++p))) {}

			return Token{ TokenCategory::IntLiteral,{ begin, distance(begin, p) } };
		}

		// error
		throw 0;
	}

	// Implementation of TokenSource
	//
	TokenSource::TokenSource(string_view file)
		: file_(file), index_(0) { }

	Token TokenSource::Peek(int lookahead)
	{
		PrepareBuffer(lookahead);
		
		if (buffer_.size() > lookahead)
		{
			return buffer_[lookahead];
		}
		else
		{
			return CreateEofToken();
		}
	}

	Token TokenSource::Consume()
	{
		PrepareBuffer(0);

		if (!buffer_.empty())
		{
			auto result = buffer_.front();
			buffer_.pop_front();

			return result;
		}
		else
		{
			return CreateEofToken();
		}
	}

	Token TokenSource::CreateEofToken()
	{
		auto content = string_view{ file_.data() + file_.length(), 0 };

		return Token{ TokenCategory::EndOfFile, content };
	}

	void TokenSource::PrepareBuffer(int lookahead)
	{

	}
}