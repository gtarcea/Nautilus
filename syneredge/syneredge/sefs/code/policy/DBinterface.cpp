
#include <PolUtil.hpp>
#include <DBexception.hpp>
#include <DBinterface.hpp>

// ------------------------------------------------------------------------

/**
 * Open the database.
 *
 * @throw DBexception if there was an error.
 */

DBinterface::DBinterface()
{
    char *errorMessage = DBmysql_real_connect(&mysql);
    if (errorMessage != NULL) {
        char *msg = new char[100+strlen(errorMessage)];
        DBexception *ex = new DBexception(msg);
        throw ex;
    }
    result = NULL;
    int numColumns = -1;
    latestQuery = new string("");
    queryAndError = new string("");;
}

// ------------------------------------------------------------------------

/**
 * Execute a database command.  Close and open database to
 * avoid command interaction.
 *
 * @param command Text command without trailing ';'.
 *
 * @return True on success.  On failure, use
 * <code>error</code> call to get specifics.
 */
bool DBinterface::execute(const char *command)
{
    (*latestQuery) = command;
    // printf("++++ DBinterface execute : %s\n", command);
    // clear out old result if there was one.
    if (result != NULL) {
        DBmysql_free_result(result);
        result = NULL;
    }

    bool ret = DBmysql_query(mysql, command) == 0;
    if (ret) {
        result = DBmysql_store_result(mysql);
        if (result != NULL) {
            // there was data from the query, find the number of fields
            numColumns = DBmysql_num_fields(result);
            numRows = DBmysql_num_rows(result);
        }
    }
    return ret;
}

// ------------------------------------------------------------------------

/**
 * Get the number of columns returned by the query.  This
 * should only be necessary if a wildcard ('*') were specified
 * in the SELECT statement.
 *
 * @return Number of columns, or -1 on error.
 */
int DBinterface::getNumColumns()
{
    if (result == NULL) {
        return -1;
    }
    return DBmysql_num_fields(result);
}

// ------------------------------------------------------------------------

/**
 * Get the number of rows returned by the query.
 *
 * @return Number of rows, or -1 on error.
 */
int DBinterface::getNumRows()
{
    if (result == NULL) {
        return -1;
    }
    return DBmysql_num_rows(result);
}

// ------------------------------------------------------------------------

/*
 * Get the data from the given row and column.  Return NULL
 * if the row or col is out of range.
 *
 * @param row Zero relative row index.
 *
 * @param col Zero relative column index.
 *
 * @return Text from the requested column or NULL if col is
 *         out of bounds or there is some other error.
 *
 */
const char *DBinterface::get(int row, int col)
{
    if ((result == NULL) ||
        (row < 0) || (row >= numRows) ||
        (col < 0) || (col >= numColumns)) {
        return NULL;
    }
    DBmysql_data_seek(result, row);
    DBmysql_row dbRow = DBmysql_fetch_row(result);
    return DBcolumnContent(dbRow, col);
}

// ------------------------------------------------------------------------

/**
 * Get the error from the most recent query.
 *
 * @return The most recent query concatenated with the error message.
 */

const char *DBinterface::error(void)
{
    (*queryAndError) = (*latestQuery);
    (*queryAndError) += " : ";
    (*queryAndError) += DBmysql_error(mysql);
    return queryAndError->c_str();
}

// ------------------------------------------------------------------------

/**
 * If the database was open, then close it.
 */
DBinterface::~DBinterface(void)
{
    if (dbOpen) {
        DBmysql_close(mysql);
        mysql = NULL;
        result = NULL;
        dbOpen = false;
    }
    delete latestQuery;
    delete queryAndError;
}
