
#ifndef __seerror_include_
#define __seerror_include_

#include <stdexcept>
#include <string>
#include <iostream>
using namespace std ;

class seerror : public std::runtime_error
{
public:
	seerror(string error) : std::runtime_error(error) {}
} ;

#endif /* __seerror_include_ */

