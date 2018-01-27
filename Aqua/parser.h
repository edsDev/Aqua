#pragma once
#include "ast-info.h"

namespace eds::aqua::compiler
{
	std::unique_ptr<ModuleDef> ParseFile(std::string_view file);
}