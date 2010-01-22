
#include <PolFileSystem.hpp>

// ------------------------------------------------------------------------

PolFileSystem::PolFileSystem(void)
{
    db = NULL;
    errorMessage = "";
    selected = false;
}

// ------------------------------------------------------------------------

/**
 * Clean up.
 */
PolFileSystem::~PolFileSystem(void)
{
    if (db != NULL) {
        delete db;
        db = NULL;
    }
}

// ------------------------------------------------------------------------

/**
 * Initialize local variables.
 *
 * @return True on success.
 */
bool PolFileSystem::initialize(void)
{
    errorMessage = "";
    if (db == NULL) {
        try {
            db = new DBinterface();
        }
        catch (DBexception *ex) {
            errorMessage = "Unable to open database: ";
            errorMessage += ex->toString();
            return false;
        }
    }
    return true;
}

// ------------------------------------------------------------------------

/**
 * If some database error occurred and no other error was
 * reported, then report this one.
 *
 * @param msg Message prefix.
 *
 * @param ok If false, then report an error.
 */

void PolFileSystem::setDefaultError(const char *msg, bool ok)
{
    if (!ok) {
        if (errorMessage == "") {
            errorMessage = msg;
            errorMessage += " : ";
            errorMessage += db->error();
        }
    }
}

// ------------------------------------------------------------------------

/**
 * Determine if this is a valid name for a new file system. 
 * It must not already be in the database and must not be
 * empty or too long.
 *
 * @param fileSystemName Name of new virtual disk.
 *
 * @return True if ok, false if not.
 */
bool PolFileSystem::validateNewPolFileSystem(const char *fileSystemName)
{
    if (!CheckFieldSize(fileSystemName, &errorMessage,
        sizeof(SEDB_file_system__name), "file system name")) {
        return false;
    }

    // check to see if there already is an entry.
    char query[200+strlen(fileSystemName)];
    sprintf(query, "SELECT name FROM file_system WHERE name = '%s'", fileSystemName);
    if (db->execute(query)) {
        if (db->getNumRows() > 0) {
            errorMessage = "Can not insert file system.";
            errorMessage += "  There is already a file system entry by the name of '";
            errorMessage += fileSystemName;
            errorMessage += "'.";
            return false;
        }
        else {
            return true;
        }
    }
    return false;
}

// ------------------------------------------------------------------------

/**
 * Ensure that the virtual disks listed exist.
 *
 * @param virtualDisks List of virtual disks.
 */
bool PolFileSystem::virtualDisksExist(const char **virtualDisks)
{
    bool ok = true;
    char query[200+sizeof(SEDB_virtual_disk__name)];
    for (int v = 0; ok && (virtualDisks[v] != NULL); v++) {
        const char *name = virtualDisks[v];
        if (strlen(name) >= (sizeof(SEDB_virtual_disk__name) - 1)) {
            errorMessage = "No such virtual disk: ";
            errorMessage += name;
            ok = false;
        }
        sprintf(query, "SELECT name FROM virtual_disk WHERE name = '%s'", name);
        ok = ok && db->execute(query);
        if (ok) {
            ok = ok && (db->getNumRows() != 0);
            if (!ok) {
                errorMessage = "Virtual disk '";
                errorMessage += name;
                errorMessage += "' does not exist.";
            }
        }
    }
    return ok;
}

// ------------------------------------------------------------------------

/**
 * Ensure that the virtual disks listed are not used by a
 * file system already.
 *
 * @param virtualDisks List of virtual disks.
 */
bool PolFileSystem::virtualDisksUnused(const char **virtualDisks)
{
    bool ok = true;
    char query[200+sizeof(SEDB_virtual_disk__name)];
    for (int v = 0; ok && (virtualDisks[v] != NULL); v++) {
        const char *name = virtualDisks[v];
        sprintf(query, "SELECT name FROM file_system WHERE virtual_disk = '%s'", name);
        ok = ok && db->execute(query);
        if (ok) {
            ok = ok && (db->getNumRows() == 0);
            if (!ok) {
                errorMessage = "Virtual disk ";
                errorMessage += name;
                errorMessage += " is already in use by file system ";
                errorMessage += db->get(0,0);
            }
        }
    }
    return ok;
}

// ------------------------------------------------------------------------

/**
 * Insert the file system into the database.
 *
 * @param fileSystemName Name of new file system.
 *
 * @param virtualDisks Null terminated list of virtual disks.
 *
 * @return True on success, false on failure.
 */

bool PolFileSystem::insert(const char *fileSystemName, const char **virtualDisks)
{
    bool ok = true;
    const char *name = virtualDisks[0];
    char query[200 +
        sizeof(SEDB_file_system__name) +
        sizeof(SEDB_file_system__virtual_disk)];

    for (int vd = 0; ok && ((name = virtualDisks[vd]) != NULL); vd++) {
        sprintf(query, "INSERT INTO file_system VALUES ('%s', '%s')",
            fileSystemName, name);
        ok = db->execute(query);
    }
    return ok;
}

// ------------------------------------------------------------------------

/**
 * Add a new file system.  Make sure that the name is valid,
 * the name is not already used, and the virtual disks exist
 * and are unused.
 *
 * Handle this as a database transaction - if anything goes
 * wrong, then rollback all database actions.
 *
 * @param fileSystemName Name for referring to this file system.
 *
 * @param virtualDisks Null terminated list of names of virtual disks to use.
 *
 * @return True on success, false on failure.
 */
bool PolFileSystem::Insert(const char *fileSystemName, const char **virtualDisks)
{
    // ensure we have the fields correct
    sizeof(SEDB_virtual_disk__name);
    selected = false;

    bool ok = initialize();   // return status

    ok = ok && db->execute("SET AUTOCOMMIT=0");

    ok = ok && db->execute("START TRANSACTION");

    // ensure that the name is ok and does not already exist
    ok = ok && validateNewPolFileSystem(fileSystemName);

    // make sure all of the virtual disks exist
    ok = ok && virtualDisksExist(virtualDisks);

    // make sure that none of the virtual disks are in use by another file system
    ok = ok && virtualDisksUnused(virtualDisks);

    // insert the records
    ok = ok && insert(fileSystemName, virtualDisks);

    setDefaultError("Unable to insert file system", ok);

    ok = ok && db->execute("COMMIT");

    if (!ok) {
        db->execute("ROLLBACK");
    }

    return ok;
}

// ------------------------------------------------------------------------

/**
 * Delete the given file system.  Fail if it does not exist. 
 * Handle this as a database transaction - if anything goes
 * wrong, then rollback all database actions.
 *
 * @parameter fileSystemName Name of file system to delete. 
 *
 * @return Return true on success, false on failure.
 */

bool PolFileSystem::Delete(const char *fileSystemName)
{
    bool ok = initialize();    // return status
    selected = false;

    // (over)allocate enough space for all queries.
    char query[200 + sizeof(SEDB_file_system__name)];

    if (strlen(fileSystemName) >= (sizeof(SEDB_file_system__name)-1)) {
        ok = false;
        errorMessage = "No such file system ";
        errorMessage += fileSystemName;
    }

    ok = ok && db->execute("SET AUTOCOMMIT=0");

    ok = ok && db->execute("START TRANSACTION");

    if (ok) {
        sprintf(query,
            "SELECT name FROM file_system WHERE name = '%s'",
            fileSystemName);
    }

    ok = ok && db->execute(query);

    if (ok) {
        if (db->getNumRows() == 0) {
            ok = false;
            errorMessage = "No such file system ";
            errorMessage += fileSystemName;
        }
    }

    if (ok) {
        sprintf(query, "DELETE FROM file_system WHERE name = '%s'", fileSystemName);
        ok = db->execute(query);
    }

    ok = ok && db->execute("COMMIT");

    setDefaultError("Unable to delete file system", ok);

    if (!ok) {
        db->execute("ROLLBACK");
    }

    return ok;
}

// ------------------------------------------------------------------------

/**
 * Perform a select on the database, fetching all columns of all rows.
 *
 * @return True on success.
 */

bool PolFileSystem::Select(void)
{
    bool ok = initialize();

    sizeof(SEDB_file_system__name);          // assert that we have the right table name
    sizeof(SEDB_file_system__virtual_disk);  // assert that we have the right field name

    if (ok && db->execute("SELECT * FROM file_system ORDER BY name,virtual_disk")) {
        selected = true;
        ok =  true;
    }
    setDefaultError("Unable to selec file system", ok);
    return ok;
}

// ------------------------------------------------------------------------

/**
 * Get the configuration for this file system.
 *
 * @param fileSystemName Name of file system to delete. 
 *
 * @param config Put configuration here.
 *
 * @return True on success, false on failure.
 */

bool PolFileSystem::Config(const char *fileSystemName, string *config)
{
     bool ok = initialize();   
     selected = false;

     // build config in cfg
     string cfg = "\n";
     cfg += fileSystemName;
     cfg += "\n";

     // build database queries here.  Make sure it is big enough.
     char query[200 +
         sizeof(SEDB_virtual_disk__name) +
         sizeof(SEDB_file_system__virtual_disk) +
         sizeof(SEDB_file_system__name)];

     sizeof(SEDB_virtual_disk__host);
     sizeof(SEDB_virtual_disk__directory);
     sizeof(SEDB_virtual_disk__bytes);

     // build a list of virtual disks
     if (ok) {
         sprintf(query, "SELECT virtual_disk FROM file_system WHERE name = '%s'",
             fileSystemName);
         ok = db->execute(query);
         if (ok) {
             int numVirtDisk = db->getNumRows();
             if (numVirtDisk > 0) {
                 // get the list of virtual disks
                 string virtualDisks[numVirtDisk];
                 for (int row = 0; row < numVirtDisk; row++) {
                     virtualDisks[row] = db->get(row, 0);
                 }

                 // get the block size for each virtual disk
                 string blockSizes[numVirtDisk];
                 for (int row = 0; ok && (row < numVirtDisk); row++) {
                     sprintf(query,
                         "SELECT block_size FROM virtual_disk WHERE name = '%s'",
                         virtualDisks[row].c_str());
                     ok = db->execute(query);
                     if (ok) {
                         blockSizes[row] = db->get(row, 0);
                     }
                 }

                 for (int row = 0; ok && (row < numVirtDisk); row++) {
                     cfg += "\n";
                     cfg += virtualDisks[row].c_str();
                     cfg += " ";
                     cfg += blockSizes[row].c_str();
                     cfg += "\n\n";
                     sprintf(query,
                         "SELECT host,directory,bytes FROM virtual_disk WHERE name = '%s'",
                         virtualDisks[row].c_str());
                     
                     int32 blkSiz = atol(blockSizes[row].c_str());
                     ok = db->execute(query);
                     if (ok) {
                         int numBlockFile = db->getNumRows();
                         for (int bf = 0; bf < numBlockFile; bf++) {
                             cfg += "    ";
                             cfg += db->get(bf, 0);
                             cfg += " ";
                             cfg += db->get(bf, 1);
                             cfg += " ";
                             int64 numBlocks = atoll(db->get(bf, 2));
                             numBlocks = numBlocks / blkSiz;
                             char nb[200];
                             sprintf(nb, "%Ld", numBlocks);
                             cfg += nb;
                             cfg += "\n";
                         }
                     }
                 }

            }
            else {
                ok = false;
                errorMessage = "No such file system ";
                errorMessage += fileSystemName;
            }
        }
    }

    if (ok) {
        *config = cfg;
    }

    setDefaultError("Unable to show file system configuration", ok);

    return ok;
}

// ------------------------------------------------------------------------

/**
 * Get selected item from table.
 *
 * @param row Zero relative row in table.
 *
 * @param wol Zero relative column in table.
 *
 * @return Value at given row and column, or NULL if it does not exist.
 */
const char *PolFileSystem::get(int row, int col)
{
    if (!initialize()) {
        return false;
    }

    // if a select was not performed, then do so.
    if (!selected) {
        if (!Select()) {
            return false;
        }
    }

    return db->get(row, col);
}

// ------------------------------------------------------------------------

/**
 * Get the most recent error.  If no error, return and empty string.
 *
 * @return Error message, or empty string if none.
 */
const char *PolFileSystem::Error(void)
{
    return errorMessage.c_str();
}

