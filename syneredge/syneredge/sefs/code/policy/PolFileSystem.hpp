
#ifndef POL_FILE_SYSTEM_HPP
#define POL_FILE_SYSTEM_HPP

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>

#include <DBinterface.hpp>
#include <DBexception.hpp>
#include <PolUtil.hpp>
#include <SEDB.h>

// ------------------------------------------------------------------------

class PolFileSystem
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

    bool initialize(void);
    void setDefaultError(const char *msg, bool ok);
    bool validateNewPolFileSystem(const char *fileSystemName);
    bool virtualDisksExist(const char **virtualDisks);
    bool virtualDisksUnused(const char **virtualDisks);
    bool insert(const char *fileSystemName, const char **virtualDisks);

  public:

    PolFileSystem(void);
    ~PolFileSystem(void);
    bool Insert(const char *fileSystemName, const char **virtualDisks);
    bool Delete(const char *fileSystemName);
    bool Select(void);
    bool Config(const char *fileSystemName, string *config);
    const char *get(int row, int col);
    const char *Error(void);


};

#endif
