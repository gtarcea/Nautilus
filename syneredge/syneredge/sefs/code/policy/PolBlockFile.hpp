
#ifndef POL_BLOCKFILE_H
#define POL_BLOCKFILE_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>

#include <DBinterface.hpp>
#include <DBexception.hpp>
#include <PolUtil.hpp>
#include <SEDB.h>

class PolBlockFile
{
  private:
    /** database handle */
    DBinterface *db;

    /** contains latest error message */
    string errorMessage;

    /** True if a Select has been performed and the result is
     * still valid.  Any other query executed on the database will
     * invalidate the result and set this to false.
     */
    bool selected;

  public:

    PolBlockFile(void);
    ~PolBlockFile(void);
    bool initialize(void);
    bool Insert(const char **fields);
    bool Delete(const char *host, const char *directory);
    bool Select(void);
    const char *get(int row, int col);
    const char *Error(void);

};

#endif
