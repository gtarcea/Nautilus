
#ifndef DBINTERFACE_HPP
#define DBINTERFACE_HPP

using namespace std;

#include <string.h>
#include <SEmysql.h>

// define C interfaces to database functions
extern "C"
{
    char *DBmysql_real_connect(DBmysql *ms);
    int DBmysql_query(DBmysql dbMysql, const char *statement);
    void DBmysql_close(DBmysql dbMysql);
    const char *DBmysql_error(DBmysql dbMysql);
    DBmysql_row DBmysql_fetch_row(DBmysql_res dbResult);
    unsigned int DBmysql_num_fields(DBmysql_res dbResult);
    unsigned int DBmysql_num_rows(DBmysql_res dbResult);
    DBmysql_res DBmysql_store_result(DBmysql dbMysql);
    const char* DBcolumnContent(DBmysql_row row, int col);
    void DBmysql_free_result(DBmysql_res result);
    void DBmysql_data_seek(DBmysql_res result, int row);
}

using namespace std;
class DBinterface
{
  private:
    /** Database handle */
    DBmysql mysql;

    /** result of SELECT statement */
    DBmysql_res result;

    /** Number of fields in each row of most recent query */
    unsigned int numColumns;

    /** Number of rows in from table of most recent query */
    unsigned int numRows;

    /** Indicates whether database is open or not. */
    bool dbOpen;

    /** Most recent query. */
    string *latestQuery;

    /** Most recent query concatenated with error. */
    string *queryAndError;

  public:

    DBinterface();
    bool execute(const char *command);
    int getNumColumns();
    int getNumRows();
    const char *get(int row, int col);
    const char *error(void);
    ~DBinterface(void);

};
#endif
