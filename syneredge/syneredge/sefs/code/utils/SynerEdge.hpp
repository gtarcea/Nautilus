#ifndef SygSynerEdge_hpp
#define SygSynerEdge_hpp

#include <iostream>

namespace SynerEdge
{

#ifdef _WIN32
typedef size_t syg_ulong_ptr;
#else
typedef unsigned long syg_ulong_ptr;
#endif

#ifdef _WIN32
#else
typedef long long int64 ;
typedef unsigned long long uint64 ;
#endif

class SynerEdgeException
{
public:
        SynerEdgeException(const std::wstring &msg_) : msg(msg_) {}
        virtual ~SynerEdgeException() {}

        virtual std::wstring getMsg() const { return msg; }
private:
        const std::wstring msg;
};

std::wostream &operator<<(std::wostream &out, const SynerEdgeException &exp);

}

#endif
