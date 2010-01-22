
#include <PolVirtualDiskPolicy.hpp>

// ------------------------------------------------------------------------

PolVirtualDiskPolicy::PolVirtualDiskPolicy(void)
{
    db = NULL;
    errorMessage = "";
    selected = false;
}

// ------------------------------------------------------------------------

/**
 * Clean up.
 */
PolVirtualDiskPolicy::~PolVirtualDiskPolicy(void)
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
bool PolVirtualDiskPolicy::initialize(void)
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
 * insert a new virtual disk policy.
 *
 * @param name Name of virtual disk policy.
 *
 * @param policy Disk policy.
 *
 * @return Return true on success, false on failure.
 */
bool PolVirtualDiskPolicy::Insert(const char *name, const char *policy)
{
    bool ok = initialize();

    ok = ok && CheckFieldSize(name, &errorMessage,
        sizeof(SEDB_virtual_disk__name), "virtual disk policy name");

    ok = ok && CheckFieldSize(policy, &errorMessage, (1<<15), "virtual disk policy");

    // determine if the policy is valid.  Try to do a query on
    // it, and if it suceeds, then it is ok.  Note that it may not
    // return any block files.
    char query[100 + strlen(policy)];
    // make sure we have the table and field names right
    sizeof(SEDB_block_file__host);
    sizeof(SEDB_virtual_disk);

    // make a query with this policy.  If it works, the policy is valid.
    if (ok) {
        sprintf(query, "SELECT host FROM block_file WHERE %s", policy);
        selected = false;
        ok = db->execute(query);

        if (!ok) {
            errorMessage =
                "Invalid policy is not a valid query for selecting block files : ";
            errorMessage += policy;
            errorMessage += " : ";
            errorMessage += db->error();
        }
    }

    if (ok) {
        // Insert it into the table.  If it works, then all is ok. 
        // If not, then some other problem, probably there is already
        // an entry by that name.
        string qPolicy = "";
        for (int c = 0; c < strlen(policy); c++) {
            if (policy[c] == '\'') {
                qPolicy += "\\'";
            }
            else {
                qPolicy += policy[c];
            }
        }

        char insert[100+strlen(name)+strlen(qPolicy.c_str())];

        sprintf(insert, "INSERT INTO virtual_disk_policy VALUES ('%s', '%s')",
            name, qPolicy.c_str());
        ok = db->execute(insert);
    }

    if (!ok) {
        if (errorMessage == "" ) {
            errorMessage = "Unable to insert: ";
            errorMessage += db->error();
        }
    }

    return ok;
}

// ------------------------------------------------------------------------

/**
 * Delete the given policy.  Before doing so, make sure that
 * no virtual disks depend on it.  Also, fail if it does not exist.
 *
 * @parameter name Name of virtual disk policy.
 *
 * @return Return true on success, false on failure.
 */

bool PolVirtualDiskPolicy::Delete(const char *name)
{
    bool ok = initialize();

    ok = ok && db->execute("SET AUTOCOMMIT=0");

    ok = ok && db->execute("START TRANSACTION");

    // allocate enough space for all queries.  Also make sure
    // that the proper database names are being used.
    char query[200 +
        sizeof(SEDB_virtual_disk__virtual_disk_policy) +
        sizeof(SEDB_virtual_disk__name) +
        sizeof(SEDB_virtual_disk_policy__name)];

    // Determine if this virtual_disk_policy is being used.  If
    // so, then do not allow it to be deleted.
    if (ok) {
        sprintf(query,
            "SELECT name FROM virtual_disk WHERE virtual_disk_policy = '%s'",
            name);
        selected = false;
        ok = db->execute(query);
    }

    if (ok) {
        if (db->getNumRows() > 0) {
            errorMessage = "Can not delete virtual disk policy ";
            errorMessage += name;
            errorMessage += " because it is in use by virtual disk ";
            errorMessage += db->get(0,0);
            ok = false;
        }
    }

    // build query to check for existance of this virtual disk policy
    if (ok) {
        sprintf(query, "SELECT * FROM virtual_disk_policy WHERE name = '%s'", name);
        ok = db->execute(query);
    }

    // if this policy does not exist, then flag an error
    if (ok) {
        if (db->getNumRows() == 0) {
            errorMessage = "No such virtual_disk_policy : ";
            errorMessage += name;
            ok = false;
        }
    }

    // build query to delete this policy
    if (ok) {
        sprintf(query, "DELETE FROM virtual_disk_policy WHERE name = '%s'", name);
        ok = db->execute(query);
    }

    // if everything went ok,then commit changes to database
    ok = ok && db->execute("COMMIT");

    // if there was a problem but no error message, then it was
    // some database access error and flag it as such.
    if (!ok) {
        if (errorMessage == "") {
            errorMessage = "Unable to delete virtual disk policy ";
            errorMessage += name;
            errorMessage += " : ";
            errorMessage += db->error();
        }
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

bool PolVirtualDiskPolicy::Select(void)
{
    if (!initialize()) {
        return false;
    }
    sizeof(SEDB_virtual_disk_policy);        // assert that we have the right table name
    sizeof(SEDB_virtual_disk_policy__name);  // assert that we have the right field name

    if (db->execute("SELECT * FROM virtual_disk_policy ORDER BY name")) {
        selected = true;
        return true;
    }
    else {
        errorMessage = "Unable to perform select on table virtual_disk_policy: ";
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
const char *PolVirtualDiskPolicy::get(int row, int col)
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
const char *PolVirtualDiskPolicy::Error(void)
{
    return errorMessage.c_str();
}

