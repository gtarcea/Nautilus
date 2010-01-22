/**
 *  File: binfo.cpp
 *  This is a generated file based on the build.
 *  Do not modify this directly, instead modify
 *  the makefile that generated it.
*/

#include "binfo.hpp"

static char *product_header[] = {
	BUILD_PRODUCT,
	BUILD_VERSION,
	BUILD_DATE,
	BUILD_OPTIM,
	BUILD_BITS,
	BUILD_OS,
	0
};

char **get_product_header() { return product_header; }

