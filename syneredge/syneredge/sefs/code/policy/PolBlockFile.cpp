
#include <PolBlockFile.hpp>

// ------------------------------------------------------------------------

PolBlockFile::PolBlockFile(void)
{
    db = NULL;
    errorMessage = "";
    selected = false;
}

// ------------------------------------------------------------------------

/**
 * Clean up.
 */
PolBlockFile::~PolBlockFile(void)
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
bool PolBlockFile::initialize(void)
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
 * Insert a new block file.
 *
 * @param fields List of fields for block file, in order. 
 *        Terminated with NULL pointer.  Fields:
 *             host
 *             directory
 *             max_bytes
 *             bytes
 *             room
 *             building
 *
 * Note that little checking is done on the "room" and
 * "building" parameters, and in fact if a user wanted to
 * modify the database to provide block drive attributes that
 * were more useful to them, they would just be appended to
 * this list.
 *
 * @return Return true on success, false on failure.
 */
bool PolBlockFile::Insert(const char **fields)
{
    bool ok = initialize();   // return status

    selected = false;

    int count = 0;
    for (int f = 0; fields[f] != NULL; f++) {
        count++;
    }

    if (count != BLOCK_FILE__NUM_FIELD) {
        char msg[200];
        sprintf(msg, "Wrong number of parameters.  %d found but %d required.",
            count, BLOCK_FILE__NUM_FIELD);
        errorMessage = msg;
        ok = false;
    }

    #define CFS(I,SIZE,NAME) CheckFieldSize(fields[(I)],&errorMessage,sizeof(SIZE),NAME)
    #define CFI(I,NAME) CheckInt64Field(fields[(I)],&errorMessage,NAME)
    ok = ok && 
        CFS(0, SEDB_block_file__host,      "host")      &&
        CFS(1, SEDB_block_file__directory, "directory") &&
        CFI(2,                             "max_bytes") &&
        CFI(3,                             "bytes")     &&
        CFS(4, SEDB_block_file__room,      "room")      &&
        CFS(5, SEDB_block_file__building,  "building");

    sizeof(SEDB_block_file);   // make sure we have the table name right

    if (ok) {
        // build up the insert string
        string query = "INSERT INTO block_file VALUES (";
        for (int f = 0; fields[f] != NULL; f++) {
            query = query + "\'";
            query = query + fields[f];
            query = query + "\'";
            if ((f+1) < BLOCK_FILE__NUM_FIELD) {
                query = query + ", ";
            }
        }
        query = query + " )";

        // execute query (insert)
        ok = db->execute(query.c_str());
        if (!ok) {
            errorMessage = "Unable to insert block file: ";
            errorMessage += db->error();
            ok = false;
        }
    }
    return ok;
}

// ------------------------------------------------------------------------

/**
 * Delete the given block file.  Before doing so, make sure that
 * no virtual disks depend on it.  Also, fail if it does not exist.
 *
 * All checks are done as a single transaction to guard
 * against the possibility of another user doing another
 * operation on this block file at the same time.
 *
 * @parameter host Name of host where block file is located.
 *
 * @parameter directory Name of directory where block file is located.
 *
 * @return Return true on success, false on failure.
 */

bool PolBlockFile::Delete(const char *host, const char *directory)
{
    bool ok = initialize();

    // (over)allocate enough space for all queries.
    char query[200 + sizeof(SEDB_block_file) + sizeof(SEDB_virtual_disk)];

    // make sure that the proper database names are being used.
    // (if incorrect, then it fails at compile time)
    sizeof(SEDB_virtual_disk__name);
    sizeof(SEDB_block_file__host);
    sizeof(SEDB_block_file__directory);

    char selection[200 + sizeof(SEDB_block_file)];

    ok = ok && db->execute("SET AUTOCOMMIT=0");

    ok = ok && db->execute("START TRANSACTION");

    if (ok) {
        sprintf(selection, "( host = '%s' ) AND ( directory = '%s' )",
            host, directory);

        // Determine if this block_file is being used.  If
        // so, then do not allow it to be deleted.
        sprintf(query, "SELECT name FROM virtual_disk WHERE %s", selection);
        selected = false;
        ok = db->execute(query);
    }

    if (ok) {
        if (db->getNumRows() > 0) {
            ok = false;
            errorMessage = "Can not delete block file ";
            errorMessage += host;
            errorMessage += ", ";
            errorMessage += directory;
            errorMessage += " because it is in use by virtual_disk ";
            errorMessage += db->get(0,0);
        }
    }
    
    // make sure that this block file exists
    if (ok) {
        sprintf(query, "SELECT * FROM block_file WHERE %s", selection);
        ok = db->execute(query);
    }

    if (ok) {
        if (db->getNumRows() == 0) {
            errorMessage = "No such block file : ";
            errorMessage += host;
            errorMessage += ", ";
            errorMessage += directory;
            ok = false;
        }
    }

    // perform the actual deletion
    if (ok) {
        sprintf(query, "DELETE FROM block_file WHERE %s", selection);
        ok = db->execute(query);
    }

    ok = ok && db->execute("COMMIT");

    if (!ok) {
        // If no custom error generated, then use the generic one.
        if (errorMessage == "") {
            errorMessage = "Unable to delete block file ";
            errorMessage += host;
            errorMessage += ", ";
            errorMessage += directory;
            errorMessage += " : ";
            errorMessage += db->error();
        }

        // Try to nullify the transaction.  If there was some
        // database access error then this does nothing.
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

bool PolBlockFile::Select(void)
{
    if (!initialize()) {
        return false;
    }
    sizeof(SEDB_block_file);            // assert that we have the right table name
    sizeof(SEDB_block_file__host);      // assert that we have the right field name
    sizeof(SEDB_block_file__directory); // assert that we have the right field name

    if (db->execute("SELECT * FROM block_file ORDER BY host, directory")) {
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
const char *PolBlockFile::get(int row, int col)
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
const char *PolBlockFile::Error(void)
{
    return errorMessage.c_str();
}

