
#include <PolVirtualDisk.hpp>

// ------------------------------------------------------------------------

PolVirtualDisk::PolVirtualDisk(void)
{
    db = NULL;
    errorMessage = "";
    selected = false;
}

// ------------------------------------------------------------------------

/**
 * Clean up.
 */
PolVirtualDisk::~PolVirtualDisk(void)
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
bool PolVirtualDisk::initialize(void)
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
 * Determine if this is a valid name for a new virtual disk. 
 * It must not already be in the database and must not be
 * empty or too long.
 *
 * @param virtDiskName Name of new virtual disk.
 *
 * @return True if ok, false if not.
 */
bool PolVirtualDisk::validateNewPolVirtualDisk(const char *virtDiskName)
{
    if (!CheckFieldSize(virtDiskName, &errorMessage,
        sizeof(SEDB_virtual_disk__name), "virtual disk name")) {
        return false;
    }

    // check to see if there already is an entry.
    char query[200+strlen(virtDiskName)];
    sprintf(query, "SELECT name FROM virtual_disk WHERE name = '%s'", virtDiskName);
    if (db->execute(query)) {
        if (db->getNumRows() > 0) {
            errorMessage = "Can not insert virtual disk.";
            errorMessage += "  There is already a virtual disk entry by the name of '";
            errorMessage += virtDiskName;
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
 * Get the policy that corresponds to the given policy name. 
 * If it does not exist return NULL.
 *
 * @param policyName Name of policy.
 *
 * @param policy Put policy here.
 *
 * @return True if all ok, false if there was a problem.
 */

bool PolVirtualDisk::getPolicy(const char* policyName, string *policy)
{
    char query[200+strlen(policyName)];
    sprintf(query, "SELECT policy FROM virtual_disk_policy WHERE name = '%s'",
        policyName);
    if (db->execute(query)) {
        if (db->getNumRows() > 0) {
            *policy = db->get(0,0);
            return true;
        }
        errorMessage = "No such policy named ";
        errorMessage += policyName;
        return false;
    }
    errorMessage = "Unable to query database with ";
    errorMessage += query;
    return false;
}

// ------------------------------------------------------------------------


/**
 * Make sure that the number of bytes required and the block size are ok.
 *
 * @param bytes Number of bytes required for virtual disk.
 *
 * @param blockSize Block size in bytes.
 *
 * @param b Number of bytes required for virtual disk (returned).
 *
 * @param bs Block size in bytes (returned).
 *
 * @return True if ok, false if some error.
 */
bool PolVirtualDisk::validateBytes(const char *bytes, const char *blockSize,
                                int64 *b, int32 *bs)
{
    *b = atoll(bytes);
    *bs = atoi(blockSize);

    if (*bs < MIN_BLOCK_SIZE) {
        char msg[200];
        sprintf(msg,
            "Invalid block size or block size too small.  Minimum block size: %d.",
            MIN_BLOCK_SIZE);
        errorMessage = msg;
        return false;
    }

    int32 numBlocks = (*b) / (*bs);
    if (numBlocks < MIN_NUMBER_OF_BLOCKS) {
        char msg[200];

        sprintf(msg, "Virtual disk size too small.  You must use at least %d blocks but you only used %d blocks.", MIN_NUMBER_OF_BLOCKS, numBlocks);
        errorMessage = msg;
        return false;
    }

    // no errors found
    return true;
}

// ------------------------------------------------------------------------


/**
 * Get a list of block files to be used in making the virtual disk drive.
 *
 * @param requiredKbytes Number of kilobytes required.
 *
 * @param policy Policy dictating what kind of disks can be used.
 *
 * @return List of blockf files to be used or NULL if some problem.
 */
VDBlockFileList *PolVirtualDisk::getDiskList(const int64 requiredKbytes, const char *policy)
{
    sizeof(SEDB_block_file__host);    // ensure that we have the correct field name
    sizeof(SEDB_block_file__directory);    // ensure that we have the correct field name
    char query[200 + strlen(policy)];
    int64 totalKbytes = 0;
    sizeof(SEDB_block_file);        // make sure we have the right name
    VDBlockFileList *list = NULL;   // return value
    sprintf(query, "SELECT host,directory,bytes FROM block_file WHERE %s", policy);
    int64 kb;
    if (db->execute(query)) {
        list = new VDBlockFileList();
        int row = 0;
        while (db->get(row, 0) && (totalKbytes < requiredKbytes)) {
            // only look at disks with unused space
            kb = atoll(db->get(row,2));
            if (kb > 0) {
                VDBlockFile *blockFile = new VDBlockFile();
                strcpy(blockFile->host, db->get(row, 0));
                strcpy(blockFile->directory, db->get(row, 1));
                blockFile->bytes = kb;
                blockFile->taking = -1;
                list->push_back(blockFile);
                totalKbytes += kb;
            }
            row++;
        }
        if (totalKbytes < requiredKbytes) {
            fprintf(stderr,
                "Insufficient disk space.  %Ld requested, but only %Ld available.  Short by %Ld kbytes.\n",
                requiredKbytes, totalKbytes, requiredKbytes - totalKbytes);
            while (list->size() > 0) {
                VDBlockFile *blockFile = list->back();
                list->pop_back();
                delete blockFile;
            }
            delete list;
            list = NULL;
        }
    }
    else {
        errorMessage = "Could not query for virtual disk : ";
        errorMessage += " : ";
        errorMessage += db->error();
        return NULL;
    }
    return list;
}

// ------------------------------------------------------------------------

/**
 * Add a virtual disk entry to the database.  Assume that all
 * of the block file entries have been updated.
 *
 * @param virtDiskName Name for referring to this disk.
 *
 * @param policyName Name of policy to be used when selecting block files.
 *
 * @param bs Size of blocks within block files.
 *
 * @param blockFiles List of block files to be used in the
 *        construction of this virtual disk.
 *
 * @return True on success, false on failure.
 */
bool PolVirtualDisk::insertPolVirtualDisk(const char *virtDiskName,
                                    const char *policyName,
                                    const int32 bs,
                                    const VDBlockFileList *blockFiles)
{
    // ensure we have the fields correct
    sizeof(SEDB_virtual_disk__name);
    sizeof(SEDB_virtual_disk__host);
    sizeof(SEDB_virtual_disk__directory);
    sizeof(SEDB_virtual_disk__bytes);
    sizeof(SEDB_virtual_disk__virtual_disk_policy);
    sizeof(SEDB_virtual_disk__block_size);

    // add virtual disk entry.
    char query[200 + strlen(virtDiskName) + sizeof(SEDB_virtual_disk)];
    for (int d = 0; d < blockFiles->size(); d++) {
        VDBlockFile *bf = (*blockFiles)[d];
        sprintf(query,
            "INSERT INTO virtual_disk VALUES ('%s', '%s', '%s', '%Ld', '%s', '%d')",
            virtDiskName, bf->host, bf->directory, bf->taking, policyName, bs);
        if(!db->execute(query)) {
            errorMessage = "Unable to insert virtual_disk entry ";
            errorMessage += virtDiskName;
            errorMessage += " : ";
            errorMessage += db->error();
            return false;
        }
    }
    return true;
}

// ------------------------------------------------------------------------

/**
 * Update the physical disk records with their new unused values.
 *
 * @param db Database handle.
 *
 * @param disks List of physical disks to use.
 *
 * @param requiredKbytes Total kilobytes required for virtual disk.
 *
 * @return True on success, false on failure.
 */ 
bool PolVirtualDisk::updateBlockFiles(int64 requiredKbytes, VDBlockFileList *disks)
{
    bool ok = true;
    char query[200 + sizeof(SEDB_block_file__host) + sizeof(SEDB_block_file__directory)];
    int64 used = 0;
    int64 kb = 0;
    int64 needed = 0;
    for (int d = 0; d < disks->size(); d++) {
        VDBlockFile *bf = (*disks)[d];
        needed = requiredKbytes - used;
        if (bf->bytes < needed) {
            kb = 0;   // need all of the space
            bf->taking = bf->bytes;
        }
        else {
            kb = bf->bytes - needed;   // need some of the space
            bf->taking = needed;
        }

        sizeof(SEDB_block_file__bytes);
        sizeof(SEDB_block_file__host);
        sizeof(SEDB_block_file__directory);
        sprintf(query,
            "UPDATE block_file SET bytes = '%Ld' WHERE host = '%s' AND directory = '%s'",
            kb, bf->host, bf->directory);
        if(!db->execute(query)) {
            errorMessage = "Unable to update block file : ";
            errorMessage += db->error();
            ok = false;
            break;
        }
        used += bf->taking;
    }
    return ok;
}

// ------------------------------------------------------------------------

/**
 * Insert a new virtual disk.  Require:
 *      - name must be unique
 *      - the virtual disk policy must exist
 *      - enough disk space
 *      - block size >= 256 bytes
 *
 * @param virtDiskName Name to be given to this virtual disk.
 *
 * @param policyName Name of policy to use.
 *
 * @param bytes Total size of virtual disk.
 *
 * @param blockSize Size of blocks within virtual disk.
 *
 * @return Return true on success, false on failure.
 */
bool PolVirtualDisk::Insert(const char *virtDiskName,
                         const char *policyName,
                         const char *bytes,
                         const char *blockSize)
{
    bool ok = true;    // return status.  Assume it is good.
    string policy;     // block file selection policy

    ok = ok && initialize();

    #define CFS(FLD,SIZE,NAME) CheckFieldSize((FLD),&errorMessage,sizeof(SIZE),NAME)

    ok = ok && CheckFieldSize(virtDiskName, &errorMessage,
            sizeof(SEDB_virtual_disk__name), "virtual disk name");
    ok = ok && CheckFieldSize(policyName, &errorMessage,
            sizeof(SEDB_virtual_disk__virtual_disk_policy), "virtual disk policy");
    ok = ok && CheckInt64Field(bytes, &errorMessage, "bytes");
    ok = ok && CheckInt32Field(blockSize, &errorMessage, "block size");


    ok = ok && db->execute("SET AUTOCOMMIT=0");

    ok = ok && db->execute("START TRANSACTION");

    selected = false;

    sizeof(SEDB_block_file);   // make sure we have the table name right

    // Check that name is valid.  Must not be too long and must
    // not already exist.
    ok = ok && validateNewPolVirtualDisk(virtDiskName);
    ok = ok && getPolicy(policyName, &policy);

    VDBlockFileList *disks = NULL;
    int64 b = atoll(bytes);
    int32 bs = atol(blockSize);
    ok = ok && validateBytes(bytes, blockSize, &b, &bs);

    ok = ok && ((disks = getDiskList(b, policy.c_str())) != NULL);

    // change block file allocation
    ok = ok && updateBlockFiles(b, disks);

    // add the virtual disk entry to the database
    ok = ok && insertPolVirtualDisk(virtDiskName, policyName, bs, disks);

    // if everything went ok,then commit changes to database
    ok = ok && db->execute("COMMIT");

    if (!ok) {
        if (errorMessage == "") {
            errorMessage = "Unable to insert vitual_disk ";
            errorMessage += virtDiskName;
            errorMessage += " : ";
            errorMessage += db->error();
        }
        db->execute("ROLLBACK");
    }

    return ok;
}

// ------------------------------------------------------------------------

/**
 * Delete the given virtual disk.  Before doing so, make sure
 * that no file systems depend on it.  Also, fail if it does
 * not exist.  If the deletion is a go, then also update each
 * block file record to reflect the fact that disk space is
 * being returned to it.
 *
 * @parameter host Name of host where block file is located.
 *
 * @parameter directory Name of directory where block file is located.
 *
 * @return Return true on success, false on failure.
 */

bool PolVirtualDisk::Delete(const char *name)
{
    bool ok = true;    // return status
    if (!initialize()) {
        ok = false;
    }
    selected = false;

    // (over)allocate enough space for all queries.
    char query[200 + sizeof(SEDB_block_file) + sizeof(SEDB_virtual_disk)];

    // make sure that the proper database names are being used.
    // (if incorrect, then it fails at compile time)
    sizeof(SEDB_virtual_disk__name);
    sizeof(SEDB_block_file__host);
    sizeof(SEDB_block_file__directory);

    ok = ok && db->execute("SET AUTOCOMMIT=0");

    ok = ok && db->execute("START TRANSACTION");

    // Determine if this block_file is being used.  If
    // so, then do not allow it to be deleted.
    if (ok) {
        sprintf(query, "SELECT name FROM file_system WHERE virtual_disk = '%s'", name);
        ok = ok && db->execute(query);
    }

    if (ok) {
        if (db->getNumRows() > 0) {
            errorMessage = "Can not delete virtual_disk";
            errorMessage += name;
            errorMessage += " because it is in use by file_system ";
            errorMessage += db->get(0,0);
            ok = false;
        }
    }

    if (ok) {
        sizeof(SEDB_virtual_disk__host);
        sizeof(SEDB_virtual_disk__directory);
        sprintf(query, "SELECT bytes,host,directory FROM virtual_disk WHERE name = '%s'", name);
        ok = ok && db->execute(query);
    }

    // if there is no such entry, then tell the user.
    if (ok  && db->getNumRows() == 0) {
        errorMessage = "No such virtual_disk : ";
        errorMessage += name;
        ok = false;
    }
    if (ok) {
        // return (reallocate) the storage back to the block drives
        int numRows = db->getNumRows();
        VDBlockFileList list;
        for (int row = 0; ok && (row < numRows); row++) {
            VDBlockFile *blockFile = new VDBlockFile();
            blockFile->bytes = atoll(db->get(row, 0));
            strcpy(blockFile->host, db->get(row, 1));
            strcpy(blockFile->directory, db->get(row, 2));
            list.push_back(blockFile);
        }
        for (int row = 0; ok && (row < numRows); row++) {
            VDBlockFile *blockFile = list[row];
            sprintf(query, "UPDATE block_file SET bytes = ( bytes + %Ld ) where host = '%s' AND directory = '%s'",
                blockFile->bytes, blockFile->host, blockFile->directory);
            ok = db->execute(query);
        }
        while (list.size() > 0) {
            VDBlockFile *blockFile = list.back();
            list.pop_back();
            delete blockFile;
        }

        if (ok) {
            sprintf(query, "DELETE FROM virtual_disk WHERE name = '%s'", name);
            ok = ok && db->execute(query);
        }
    }

    // if everything went ok,then commit changes to database
    ok = ok && db->execute("COMMIT");

    if (!ok) {
        if (errorMessage == "") {
            errorMessage = "Unable to delete vitual_disk ";
            errorMessage += name;
            errorMessage += " : ";
            errorMessage += db->error();
        }

        // if there was an error, then do not commit changes to database
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

bool PolVirtualDisk::Select(void)
{
    if (!initialize()) {
        return false;
    }
    sizeof(SEDB_virtual_disk);            // assert that we have the right table name
    sizeof(SEDB_virtual_disk__name);      // assert that we have the right field name
    sizeof(SEDB_virtual_disk__host);      // assert that we have the right field name
    sizeof(SEDB_virtual_disk__directory); // assert that we have the right field name

    if (db->execute("SELECT * FROM virtual_disk ORDER BY name, host, directory")) {
        selected = true;
        return true;
    }
    else {
        errorMessage = "Unable to perform select on table block_file: ";
        errorMessage += db->error();
    }
    return false;
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
const char *PolVirtualDisk::get(int row, int col)
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
const char *PolVirtualDisk::Error(void)
{
    return errorMessage.c_str();
}

