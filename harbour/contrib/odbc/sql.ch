/*
*
*  SQL.CH
*  (Not Ready) Headers for ODBC
*
**/

/* RETCODEs */
#define SQL_INVALID_HANDLE              (-2)
#define SQL_ERROR                       (-1)
#define SQL_SUCCESS                     0
#define SQL_SUCCESS_WITH_INFO           1
#define SQL_NO_DATA_FOUND               100

/* Standard SQL datatypes, using ANSI type numbering */
#define SQL_CHAR                        1
#define SQL_NUMERIC                     2
#define SQL_DECIMAL                     3
#define SQL_INTEGER                     4
#define SQL_SMALLINT                    5
#define SQL_FLOAT                       6
#define SQL_REAL                        7
#define SQL_DOUBLE                      8
#define SQL_VARCHAR                     12

#define SQL_TYPE_NULL                   0
#define SQL_TYPE_MIN                                    SQL_BIT
#define SQL_TYPE_MAX                    SQL_VARCHAR
#define SQL_ALL_TYPES                                   0

/* NULL status constants.  These are used in SQLColumns, SQLColAttributes,
SQLDescribeCol, SQLDescribeParam, and SQLSpecialColumns to describe the
nullablity of a column in a table. */
#define SQL_NO_NULLS                    0
#define SQL_NULLABLE                    1
#define SQL_NULLABLE_UNKNOWN            2

/* Special length values */
#define SQL_NULL_DATA                   (-1)
#define SQL_DATA_AT_EXEC                (-2)
#define SQL_NTS                         (-3)

/* SQLFreeStmt defines */
#define SQL_CLOSE                       0
#define SQL_DROP                        1
#define SQL_UNBIND                      2
#define SQL_RESET_PARAMS                3

/* SQLTransact defines */
#define SQL_COMMIT                      0
#define SQL_ROLLBACK                    1

/* SQLColAttributes defines */
#define SQL_COLUMN_COUNT                0
#define SQL_COLUMN_NAME                 1
#define SQL_COLUMN_TYPE                 2
#define SQL_COLUMN_LENGTH               3
#define SQL_COLUMN_PRECISION            4
#define SQL_COLUMN_SCALE                5
#define SQL_COLUMN_DISPLAY_SIZE         6
#define SQL_COLUMN_NULLABLE             7
#define SQL_COLUMN_UNSIGNED             8
#define SQL_COLUMN_MONEY                9
#define SQL_COLUMN_UPDATABLE            10
#define SQL_COLUMN_AUTO_INCREMENT       11
#define SQL_COLUMN_CASE_SENSITIVE       12
#define SQL_COLUMN_SEARCHABLE           13
#define SQL_COLUMN_TYPE_NAME            14
#define SQL_COLUMN_TABLE_NAME           15
#define SQL_COLUMN_OWNER_NAME           16
#define SQL_COLUMN_QUALIFIER_NAME       17
#define SQL_COLUMN_LABEL                18
#define SQL_COLATT_OPT_MAX              SQL_COLUMN_LABEL
#define SQL_COLUMN_DRIVER_START         1000

#define SQL_COLATT_OPT_MIN              SQL_COLUMN_COUNT

/* SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE */
#define SQL_ATTR_READONLY               0
#define SQL_ATTR_WRITE                  1
#define SQL_ATTR_READWRITE_UNKNOWN      2

/* SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE */
/* These are also used by SQLGetInfo                     */
#define SQL_UNSEARCHABLE                0
#define SQL_LIKE_ONLY                   1
#define SQL_ALL_EXCEPT_LIKE             2
#define SQL_SEARCHABLE                  3

/* SQLError defines */
#define SQL_NULL_HENV                   0
#define SQL_NULL_HDBC                   0
#define SQL_NULL_HSTMT                  0

#define SQL_FETCH_NEXT                   1
#define SQL_FETCH_FIRST                  2
#define SQL_FETCH_LAST                   3
#define SQL_FETCH_PRIOR                  4
#define SQL_FETCH_ABSOLUTE               5
#define SQL_FETCH_RELATIVE               6
#define SQL_FETCH_BOOKMARK               8

