
#include <mysql.h>
#include <stdio.h>
#include <stdlib.h>

#include <SEmysql.h>


// ------------------------------------------------------------------------

/** host on which the database resides */
#define DB_HOST "go"

/**
 * User ID to use when accessing the database.  This user
 * has been given authority with the mysql GRANT statement
 */
#define USER_ID "irrer"

/** Password to use when accessing the database */
#define PASSWORD ""

/** Database name */
#define DB_NAME "sedb"

// ------------------------------------------------------------------------

/**
 * Initialize the database and malloc a database handle.
 *
 * @param ms Allocated pointer to database handle or null on failure.
 *
 * @return Malloced error message is something went wrong, or
 *         NULL if ok.  The caller should free this.
 */

char *DBmysql_real_connect(DBmysql *ms)
{
    MYSQL *mysql = mysql_init(NULL);

    if (mysql_real_connect(
        mysql,
        DB_HOST,
        USER_ID,
        PASSWORD,
        DB_NAME,
        0,
        NULL,
        0) == NULL)
   {
       char *msg = malloc(100 + strlen(mysql_error(mysql)));
       sprintf(msg,
           "Error connecting to database: %s\n", mysql_error(mysql));
       return msg;
   }
   *ms = mysql;
   return NULL;
}

// ------------------------------------------------------------------------

/**
 * Send a command to the database.
 *
 * @param dbMysql Database handle.
 *
 * @param statement Command line.
 *
 * @return zero on success, 1 on failure.
 */
int DBmysql_query(DBmysql dbMysql, const char *statement)
{
    mysql_query((MYSQL*)dbMysql, statement);
}

// ------------------------------------------------------------------------

/**
 * Close the database.
 *
 * @param dbMysql Database handle constructed by <code>DBmysql_init</code>.
 */
void DBmysql_close(DBmysql dbMysql)
{
    mysql_close((MYSQL*)dbMysql);
}

// ------------------------------------------------------------------------

/**
 * Get a text version of the last error.
 *
 * @param dbMysql Database handle constructed by <code>DBmysql_init</code>.
 *
 * @return Error text.  This text must be considered a const
 *         and will be reset on the next database call.
 */
const char *DBmysql_error(DBmysql dbMysql)
{
    return mysql_error((MYSQL*)dbMysql);
}

// ------------------------------------------------------------------------

/**
 * Get a text version of the last error.
 *
 * @param dbMysql Database handle constructed by <code>DBmysql_init</code>.
 *
 * @return Error text.  This text must be considered a const
 *         and will be reset on the next database call.
 */

DBmysql_row DBmysql_fetch_row(DBmysql_res dbResult)
{
    MYSQL_ROW row = mysql_fetch_row((MYSQL_RES*)dbResult);
    return (DBmysql_row)row;
}

// ------------------------------------------------------------------------

/**
 * Get the number of fields returned by the last SELECT
 *
 * @param dbMysql Database handle constructed by <code>DBmysql_init</code>.
 *
 * @return Number of fields (columns) returned (not number of
 *         fields in table).
 */

unsigned int DBmysql_num_fields(DBmysql_res dbResult)
{
    return mysql_num_fields((MYSQL_RES*)dbResult);
}

// ------------------------------------------------------------------------

/**
 * Get the number of rows returned by the last SELECT
 *
 * @param dbMysql Database handle constructed by <code>DBmysql_init</code>.
 *
 * @return Number of rows returne.
 */

unsigned int DBmysql_num_rows(DBmysql_res dbResult)
{
    return mysql_num_rows((MYSQL_RES*)dbResult);
}

// ------------------------------------------------------------------------

/**
 * Get the result of a query.  Store them locally.
 *
 * @param dbMysql Database handle constructed by <code>DBmysql_init</code>.
 *
 * @return Result of query.
 */

DBmysql_res DBmysql_store_result(DBmysql dbMysql)
{
    MYSQL_RES *result = mysql_store_result((MYSQL*)dbMysql);
    return result;
}

// ------------------------------------------------------------------------

/**
 * Get the data from the given column in the curren tow.
 *
 * @param row Current row in table.
 *
 * @param col The zero-relative column index.
 *
 * @return Data in that column.
 */
const char* DBcolumnContent(DBmysql_row row, int col)
{
    MYSQL_ROW r = (DBmysql_row)row;
    return r[col];
}

// ------------------------------------------------------------------------

/**
 * Free the result from a query.  After an SQL query is made,
 * a result buffer is allocated.  Before another query can be
 * made, the result must be freed.
 *
 * @param result Result to free.
 *
 */
void DBmysql_free_result(DBmysql_res result)
{
    mysql_free_result((MYSQL_RES*)result);
}

// ------------------------------------------------------------------------

/**
 * Go to the given row.
 *
 * @param result Result of last query.
 *
 * @param row Seek to this row (zero relative)
 */

void DBmysql_data_seek(DBmysql_res result, int row)
{
    my_ulonglong r = row;
    mysql_data_seek((MYSQL_RES*)result, r);
}

// ------------------------------------------------------------------------
