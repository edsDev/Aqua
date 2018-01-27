#pragma once
#include <string_view>
#include <deque>

namespace eds::aqua::compiler
{
	enum class TokenCategory
	{
		// when source is exhausted
		EndOfFile,

		// meta symbol
		Comma,
		OpenParathe,
		CloseParathe,
		OpenBracket,
		CloseBracket,

		// name
		Identifier,

		// literals
		IntLiteral,
	};

	struct Token
	{
		TokenCategory category;
		std::string_view content;
	};

	class TokenSource
	{
	public:
		TokenSource(std::string_view file);

		bool Exhausted();

		Token Peek(int lookahead = 0);
		Token Consume();

	private:
		Token CreateEofToken();
		void PrepareBuffer(int lookahead);

		int index_;
		std::string_view file_;

		std::deque<Token> buffer_ = {};
	};
}