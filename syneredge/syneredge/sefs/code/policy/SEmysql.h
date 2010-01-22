
#ifndef SEmysql_INCLUDED
#define SEmysql_INCLUDED

/** portable type for MYSQL */
typedef void* DBmysql;

/** portable type for MYSQL_RES */
typedef void* DBmysql_res;

/** portable type for MYSQL_ROW */
typedef void* DBmysql_row;

#ifndef NULL
#define NULL (0)
#endif

#endif
