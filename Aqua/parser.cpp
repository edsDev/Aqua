#include "parser.h"
#include "tokenizer.h"
#include "keywords.h"

using namespace std;

namespace eds::aqua::compiler
{
	unique_ptr<Expression> ParseExpression(TokenSource& src)
	{
		auto fst_tok = src.Consume();
		if (fst_tok.category == TokenCategory::OpenParathe)
		{
			auto snd_tok = src.Consume();
			if (snd_tok.category == TokenCategory::CloseParathe)
			{
				// unit value ()
			}
			else if (snd_tok.category == TokenCategory::Identifier)
			{
				
			}
			else
			{
				throw 0;
			}
		}
		else
		{
			// literal or name
		}
	}

	unique_ptr<FunctionDef> ParseFunction(TokenSource& src)
	{

	}

	unique_ptr<ModuleDef> ParseFile(std::string_view file)
	{
		TokenSource src{ file };
		vector<unique_ptr<FunctionDef>> functions;

		while (!src.Exhausted())
		{
			if (src.Consume().category != TokenCategory::OpenParathe)
				throw 0;
			if (src.Consume().content != kAquaKeywordFun)
				throw 0;

			functions.push_back(ParseFunction(src));
		}
	}
}