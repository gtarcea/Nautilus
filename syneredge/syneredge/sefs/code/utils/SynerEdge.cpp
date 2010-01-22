#include "SynerEdge.hpp"

namespace SynerEdge
{

std::wostream &operator<<(std::wostream &out, const SynerEdgeException &exp)
{
	out << exp.getMsg();
	return out;
}

}

