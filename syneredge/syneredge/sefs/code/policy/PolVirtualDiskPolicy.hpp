
#ifndef POL_VIRTUAL_DISK_POLICY
#define POL_VIRTUAL_DISK_POLICY

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>

#include <DBinterface.hpp>
#include <DBexception.hpp>
#include <PolUtil.hpp>
#include <SEDB.h>

class PolVirtualDiskPolicy
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

    PolVirtualDiskPolicy(void);
    ~PolVirtualDiskPolicy(void);
    bool initialize(void);
    bool Insert(const char *name, const char *policy);
    bool Delete(const char *name);
    bool Select(void);
    const char *get(int row, int col);
    const char *Error(void);

};

#endif
