/*****************************************************************
** SQL.H - This is the the main include for ODBC Core functions.
**
** preconditions:
**      #include "windows.h"
**
** (C) Copyright 1990 - 1995 By Microsoft Corp.
**
**      Updated 5/12/93 for 2.00 specification
**      Updated 5/23/94 for 2.01 specification
**      Updated 11/10/94 for 2.10 specification
**      Updated 04/10/95 for 2.50 specification
*********************************************************************/

#ifndef __SQL
#define __SQL

/*
* ODBCVER                   ODBC version number (0x0200).   To exclude
*                           definitions introduced in version 2.0 (or above)
*                           #define ODBCVER 0x0100 before #including <sql.h>
*/

/* If ODBCVER is not defined, assume version 2.50 */
#ifndef ODBCVER
#define ODBCVER 0x0250
#endif

#ifdef __cplusplus
extern "C" {                /* Assume C declarations for C++   */
#endif  /* __cplusplus */

/* generally useful constants */
#if (ODBCVER >= 0x0200)
#define SQL_SPEC_MAJOR                    2     /* Major version of specification  */
#define SQL_SPEC_MINOR                    50    /* Minor version of specification  */
#define SQL_SPEC_STRING         "02.50"         /* String constant for version */
#endif  /* ODBCVER >= 0x0200 */
#define SQL_SQLSTATE_SIZE                 5     /* size of SQLSTATE */
#define SQL_MAX_MESSAGE_LENGTH  512             /* message buffer size */
#define SQL_MAX_DSN_LENGTH               32     /* maximum data source name size*/

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
#define SQL_TYPE_MIN     				SQL_BIT
#define SQL_TYPE_MAX                    SQL_VARCHAR
#define SQL_ALL_TYPES    				0

/* C datatype to SQL datatype mapping      SQL types 
										   ------------------- */
#define SQL_C_CHAR    SQL_CHAR             /* CHAR, VARCHAR, DECIMAL, NUMERIC */
#define SQL_C_LONG    SQL_INTEGER          /* INTEGER                      */
#define SQL_C_SHORT   SQL_SMALLINT         /* SMALLINT                     */
#define SQL_C_FLOAT   SQL_REAL             /* REAL                         */
#define SQL_C_DOUBLE  SQL_DOUBLE           /* FLOAT, DOUBLE                */
#define SQL_C_DEFAULT 99

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
#if (ODBCVER >= 0x0200)
#define SQL_COLUMN_TABLE_NAME           15
#define SQL_COLUMN_OWNER_NAME           16
#define SQL_COLUMN_QUALIFIER_NAME       17
#define SQL_COLUMN_LABEL                18
#define SQL_COLATT_OPT_MAX              SQL_COLUMN_LABEL
#else
#define SQL_COLATT_OPT_MAX              SQL_COLUMN_TYPE_NAME
#endif  /* ODBCVER >= 0x0200 */
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

#include "sqltypes.h"	    

#ifndef RC_INVOKED
/* Core Function Prototypes */

SQLRETURN SQL_API SQLAllocConnect(
    SQLHENV            henv,
    SQLHDBC FAR       *phdbc);

SQLRETURN SQL_API SQLAllocEnv(
    SQLHENV FAR       *phenv);

SQLRETURN SQL_API SQLAllocStmt(
    SQLHDBC            hdbc,
    SQLHSTMT FAR      *phstmt);

SQLRETURN SQL_API SQLBindCol(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       icol,
    SQLSMALLINT        fCType,
    SQLPOINTER         rgbValue,
    SQLINTEGER         cbValueMax,
    SQLINTEGER FAR    *pcbValue);

SQLRETURN SQL_API SQLCancel(
    SQLHSTMT           hstmt);

SQLRETURN SQL_API SQLColAttributes(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       icol,
    SQLUSMALLINT       fDescType,
    SQLPOINTER         rgbDesc,
    SQLSMALLINT        cbDescMax,
    SQLSMALLINT FAR   *pcbDesc,
    SQLINTEGER FAR    *pfDesc);

SQLRETURN SQL_API SQLConnect(
    SQLHDBC            hdbc,
    SQLCHAR FAR       *szDSN,
    SQLSMALLINT        cbDSN,
    SQLCHAR FAR       *szUID,
    SQLSMALLINT        cbUID,
    SQLCHAR FAR       *szAuthStr,
    SQLSMALLINT        cbAuthStr);

SQLRETURN SQL_API SQLDescribeCol(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       icol,
    SQLCHAR FAR       *szColName,
    SQLSMALLINT        cbColNameMax,
    SQLSMALLINT FAR   *pcbColName,
    SQLSMALLINT FAR   *pfSqlType,
    SQLUINTEGER FAR   *pcbColDef,
    SQLSMALLINT FAR   *pibScale,
    SQLSMALLINT FAR   *pfNullable);

SQLRETURN SQL_API SQLDisconnect(
    SQLHDBC            hdbc);

SQLRETURN SQL_API SQLError(
    SQLHENV            henv,
    SQLHDBC            hdbc,
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szSqlState,
    SQLINTEGER FAR    *pfNativeError,
    SQLCHAR FAR       *szErrorMsg,
    SQLSMALLINT        cbErrorMsgMax,
    SQLSMALLINT FAR   *pcbErrorMsg);

SQLRETURN SQL_API SQLExecDirect(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szSqlStr,
    SQLINTEGER         cbSqlStr);

SQLRETURN SQL_API SQLExecute(
    SQLHSTMT           hstmt);

SQLRETURN SQL_API SQLFetch(
    SQLHSTMT           hstmt);

SQLRETURN SQL_API SQLFreeConnect(
    SQLHDBC            hdbc);

SQLRETURN SQL_API SQLFreeEnv(
    SQLHENV            henv);

SQLRETURN SQL_API SQLFreeStmt(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       fOption);

SQLRETURN SQL_API SQLGetCursorName(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCursor,
    SQLSMALLINT        cbCursorMax,
    SQLSMALLINT FAR   *pcbCursor);

SQLRETURN SQL_API SQLNumResultCols(
    SQLHSTMT           hstmt,
    SQLSMALLINT FAR   *pccol);

SQLRETURN SQL_API SQLPrepare(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szSqlStr,
    SQLINTEGER         cbSqlStr);

SQLRETURN SQL_API SQLRowCount(
    SQLHSTMT           hstmt,
    SQLINTEGER FAR    *pcrow);

SQLRETURN SQL_API SQLSetCursorName(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCursor,
    SQLSMALLINT        cbCursor);

SQLRETURN SQL_API SQLTransact(
    SQLHENV            henv,
    SQLHDBC            hdbc,
    SQLUSMALLINT       fType);

#endif /* RC_INVOKED */

/*      Deprecrated functions from prior versions of ODBC */
#ifndef RC_INVOKED

SQLRETURN SQL_API SQLSetParam(            /*      Use SQLBindParameter */
    SQLHSTMT           hstmt,
    SQLUSMALLINT       ipar,
    SQLSMALLINT        fCType,
    SQLSMALLINT        fSqlType,
    SQLUINTEGER        cbParamDef,
    SQLSMALLINT        ibScale,
    SQLPOINTER         rgbValue,
    SQLINTEGER FAR     *pcbValue);

#endif /* RC_INVOKED */


/* Defines used by both Level 1 and Level 2 functions */

/* generally useful constants */
#define SQL_MAX_OPTION_STRING_LENGTH    256

/* Additional return codes */
#define SQL_STILL_EXECUTING     2
#define SQL_NEED_DATA           99

/* SQL extended datatypes */
#define SQL_DATE                                9
#define SQL_TIME                                10
#define SQL_TIMESTAMP                           11
#define SQL_LONGVARCHAR                         (-1)
#define SQL_BINARY                              (-2)
#define SQL_VARBINARY                           (-3)
#define SQL_LONGVARBINARY                       (-4)
#define SQL_BIGINT                              (-5)
#define SQL_TINYINT                             (-6)
#define SQL_BIT                                 (-7)

#define SQL_INTERVAL_YEAR                       (-80)
#define SQL_INTERVAL_MONTH                      (-81)
#define SQL_INTERVAL_YEAR_TO_MONTH              (-82)
#define SQL_INTERVAL_DAY                        (-83)
#define SQL_INTERVAL_HOUR                       (-84)
#define SQL_INTERVAL_MINUTE                     (-85)
#define SQL_INTERVAL_SECOND                     (-86)
#define SQL_INTERVAL_DAY_TO_HOUR                (-87)
#define SQL_INTERVAL_DAY_TO_MINUTE              (-88)
#define SQL_INTERVAL_DAY_TO_SECOND              (-89)
#define SQL_INTERVAL_HOUR_TO_MINUTE             (-90)
#define SQL_INTERVAL_HOUR_TO_SECOND             (-91)
#define SQL_INTERVAL_MINUTE_TO_SECOND           (-92)
#define SQL_UNICODE                             (-95)
#define SQL_UNICODE_VARCHAR                     (-96)
#define SQL_UNICODE_LONGVARCHAR                 (-97)
#define SQL_UNICODE_CHAR                        SQL_UNICODE

#define SQL_TYPE_DRIVER_START                   SQL_INTERVAL_YEAR
#define SQL_TYPE_DRIVER_END                     SQL_UNICODE_LONGVARCHAR


#if (ODBCVER >= 0x0200)
#define SQL_SIGNED_OFFSET       (-20)
#define SQL_UNSIGNED_OFFSET     (-22)
#endif  /* ODBCVER >= 0x0200 */

/* C datatype to SQL datatype mapping */
#define SQL_C_DATE       SQL_DATE
#define SQL_C_TIME       SQL_TIME
#define SQL_C_TIMESTAMP  SQL_TIMESTAMP
#define SQL_C_BINARY     SQL_BINARY
#define SQL_C_BIT        SQL_BIT
#define SQL_C_TINYINT    SQL_TINYINT
#if (ODBCVER >= 0x0200)
#define SQL_C_SLONG      SQL_C_LONG+SQL_SIGNED_OFFSET    /* SIGNED INTEGER   */
#define SQL_C_SSHORT     SQL_C_SHORT+SQL_SIGNED_OFFSET   /* SIGNED SMALLINT  */
#define SQL_C_STINYINT   SQL_TINYINT+SQL_SIGNED_OFFSET   /* SIGNED TINYINT   */
#define SQL_C_ULONG      SQL_C_LONG+SQL_UNSIGNED_OFFSET  /* UNSIGNED INTEGER */
#define SQL_C_USHORT     SQL_C_SHORT+SQL_UNSIGNED_OFFSET /* UNSIGNED SMALLINT*/
#define SQL_C_UTINYINT   SQL_TINYINT+SQL_UNSIGNED_OFFSET /* UNSIGNED TINYINT */
#define SQL_C_BOOKMARK   SQL_C_ULONG                     /* BOOKMARK         */
#endif  /* ODBCVER >= 0x0200 */


/* Level 1 Functions                    */

/* Special return values for SQLGetData */
#define SQL_NO_TOTAL                    (-4)

/* Defines for SQLGetFunctions */
#define SQL_API_SQLALLOCCONNECT      1    /* Core Functions           */
#define SQL_API_SQLALLOCENV          2
#define SQL_API_SQLALLOCSTMT         3
#define SQL_API_SQLBINDCOL           4
#define SQL_API_SQLCANCEL            5
#define SQL_API_SQLCOLATTRIBUTES     6
#define SQL_API_SQLCONNECT           7
#define SQL_API_SQLDESCRIBECOL       8
#define SQL_API_SQLDISCONNECT        9
#define SQL_API_SQLERROR            10
#define SQL_API_SQLEXECDIRECT       11
#define SQL_API_SQLEXECUTE          12
#define SQL_API_SQLFETCH            13
#define SQL_API_SQLFREECONNECT      14
#define SQL_API_SQLFREEENV          15
#define SQL_API_SQLFREESTMT         16
#define SQL_API_SQLGETCURSORNAME    17
#define SQL_API_SQLNUMRESULTCOLS    18
#define SQL_API_SQLPREPARE          19
#define SQL_API_SQLROWCOUNT         20
#define SQL_API_SQLSETCURSORNAME    21
#define SQL_API_SQLSETPARAM         22
#define SQL_API_SQLTRANSACT         23

#define SQL_NUM_FUNCTIONS           23

#define SQL_EXT_API_START           40

#define SQL_API_SQLCOLUMNS          40    /* Level 1 Functions        */
#define SQL_API_SQLDRIVERCONNECT    41
#define SQL_API_SQLGETCONNECTOPTION 42
#define SQL_API_SQLGETDATA          43
#define SQL_API_SQLGETFUNCTIONS     44
#define SQL_API_SQLGETINFO          45
#define SQL_API_SQLGETSTMTOPTION    46
#define SQL_API_SQLGETTYPEINFO      47
#define SQL_API_SQLPARAMDATA        48
#define SQL_API_SQLPUTDATA          49
#define SQL_API_SQLSETCONNECTOPTION 50
#define SQL_API_SQLSETSTMTOPTION    51
#define SQL_API_SQLSPECIALCOLUMNS   52
#define SQL_API_SQLSTATISTICS       53
#define SQL_API_SQLTABLES           54

#define SQL_API_SQLBROWSECONNECT    55    /* Level 2 Functions        */
#define SQL_API_SQLCOLUMNPRIVILEGES 56
#define SQL_API_SQLDATASOURCES      57
#define SQL_API_SQLDESCRIBEPARAM    58
#define SQL_API_SQLEXTENDEDFETCH    59
#define SQL_API_SQLFOREIGNKEYS      60
#define SQL_API_SQLMORERESULTS      61
#define SQL_API_SQLNATIVESQL        62
#define SQL_API_SQLNUMPARAMS        63
#define SQL_API_SQLPARAMOPTIONS     64
#define SQL_API_SQLPRIMARYKEYS      65
#define SQL_API_SQLPROCEDURECOLUMNS 66
#define SQL_API_SQLPROCEDURES       67
#define SQL_API_SQLSETPOS           68
#define SQL_API_SQLSETSCROLLOPTIONS 69
#define SQL_API_SQLTABLEPRIVILEGES  70

/*              SDK 2.0 Additions               */
#if (ODBCVER >= 0x0200)
#define SQL_API_SQLDRIVERS          71
#define SQL_API_SQLBINDPARAMETER    72
#define SQL_EXT_API_LAST            SQL_API_SQLBINDPARAMETER
#else
#define SQL_EXT_API_LAST            SQL_API_SQLTABLEPRIVILEGES
#endif  /* ODBCVER >= 0x0200 */

#define SQL_API_ALL_FUNCTIONS       0

#define SQL_NUM_EXTENSIONS (SQL_EXT_API_LAST-SQL_EXT_API_START+1)
#if (ODBCVER >= 0x0200)
#define SQL_API_LOADBYORDINAL       199
#endif  /* ODBCVER >= 0x0200 */

/* Defines for SQLGetInfo */
#define SQL_INFO_FIRST                       0
#define SQL_ACTIVE_CONNECTIONS               0
#define SQL_ACTIVE_STATEMENTS                1
#define SQL_DATA_SOURCE_NAME                 2
#define SQL_DRIVER_HDBC                      3
#define SQL_DRIVER_HENV                      4
#define SQL_DRIVER_HSTMT                     5
#define SQL_DRIVER_NAME                      6
#define SQL_DRIVER_VER                       7
#define SQL_FETCH_DIRECTION                  8
#define SQL_ODBC_API_CONFORMANCE             9
#define SQL_ODBC_VER                        10
#define SQL_ROW_UPDATES                     11
#define SQL_ODBC_SAG_CLI_CONFORMANCE        12
#define SQL_SERVER_NAME                     13
#define SQL_SEARCH_PATTERN_ESCAPE           14
#define SQL_ODBC_SQL_CONFORMANCE            15

#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

#define SQL_ACCESSIBLE_TABLES               19
#define SQL_ACCESSIBLE_PROCEDURES           20
#define SQL_PROCEDURES                      21
#define SQL_CONCAT_NULL_BEHAVIOR            22
#define SQL_CURSOR_COMMIT_BEHAVIOR          23
#define SQL_CURSOR_ROLLBACK_BEHAVIOR        24
#define SQL_DATA_SOURCE_READ_ONLY           25
#define SQL_DEFAULT_TXN_ISOLATION           26
#define SQL_EXPRESSIONS_IN_ORDERBY          27
#define SQL_IDENTIFIER_CASE                 28
#define SQL_IDENTIFIER_QUOTE_CHAR           29
#define SQL_MAX_COLUMN_NAME_LEN             30
#define SQL_MAX_CURSOR_NAME_LEN             31
#define SQL_MAX_OWNER_NAME_LEN              32
#define SQL_MAX_PROCEDURE_NAME_LEN          33
#define SQL_MAX_QUALIFIER_NAME_LEN          34
#define SQL_MAX_TABLE_NAME_LEN              35
#define SQL_MULT_RESULT_SETS                36
#define SQL_MULTIPLE_ACTIVE_TXN             37
#define SQL_OUTER_JOINS                     38
#define SQL_OWNER_TERM                      39
#define SQL_PROCEDURE_TERM                  40
#define SQL_QUALIFIER_NAME_SEPARATOR        41
#define SQL_QUALIFIER_TERM                  42
#define SQL_SCROLL_CONCURRENCY              43
#define SQL_SCROLL_OPTIONS                  44
#define SQL_TABLE_TERM                      45
#define SQL_TXN_CAPABLE                     46
#define SQL_USER_NAME                       47

#define SQL_CONVERT_FUNCTIONS               48
#define SQL_NUMERIC_FUNCTIONS               49
#define SQL_STRING_FUNCTIONS                50
#define SQL_SYSTEM_FUNCTIONS                51
#define SQL_TIMEDATE_FUNCTIONS              52

#define SQL_CONVERT_BIGINT                  53
#define SQL_CONVERT_BINARY                  54
#define SQL_CONVERT_BIT                     55
#define SQL_CONVERT_CHAR                    56
#define SQL_CONVERT_DATE                    57
#define SQL_CONVERT_DECIMAL                 58
#define SQL_CONVERT_DOUBLE                  59
#define SQL_CONVERT_FLOAT                   60
#define SQL_CONVERT_INTEGER                 61
#define SQL_CONVERT_LONGVARCHAR             62
#define SQL_CONVERT_NUMERIC                 63
#define SQL_CONVERT_REAL                    64
#define SQL_CONVERT_SMALLINT                65
#define SQL_CONVERT_TIME                    66
#define SQL_CONVERT_TIMESTAMP               67
#define SQL_CONVERT_TINYINT                 68
#define SQL_CONVERT_VARBINARY               69
#define SQL_CONVERT_VARCHAR                 70
#define SQL_CONVERT_LONGVARBINARY           71

#define SQL_TXN_ISOLATION_OPTION            72
#define SQL_ODBC_SQL_OPT_IEF                73

/*** ODBC SDK 1.0 Additions ***/
#define SQL_CORRELATION_NAME                74
#define SQL_NON_NULLABLE_COLUMNS            75

/*** ODBC SDK 2.0 Additions ***/
#if (ODBCVER >= 0x0200)
#define SQL_DRIVER_HLIB                     76
#define SQL_DRIVER_ODBC_VER                 77
#define SQL_LOCK_TYPES                      78
#define SQL_POS_OPERATIONS                  79
#define SQL_POSITIONED_STATEMENTS           80
#define SQL_GETDATA_EXTENSIONS              81
#define SQL_BOOKMARK_PERSISTENCE            82
#define SQL_STATIC_SENSITIVITY              83
#define SQL_FILE_USAGE                      84
#define SQL_NULL_COLLATION                  85
#define SQL_ALTER_TABLE                     86
#define SQL_COLUMN_ALIAS                    87
#define SQL_GROUP_BY                        88
#define SQL_KEYWORDS                        89
#define SQL_ORDER_BY_COLUMNS_IN_SELECT      90
#define SQL_OWNER_USAGE                     91
#define SQL_QUALIFIER_USAGE                 92
#define SQL_QUOTED_IDENTIFIER_CASE          93
#define SQL_SPECIAL_CHARACTERS              94
#define SQL_SUBQUERIES                      95
#define SQL_UNION                           96
#define SQL_MAX_COLUMNS_IN_GROUP_BY         97
#define SQL_MAX_COLUMNS_IN_INDEX            98
#define SQL_MAX_COLUMNS_IN_ORDER_BY         99
#define SQL_MAX_COLUMNS_IN_SELECT           100
#define SQL_MAX_COLUMNS_IN_TABLE            101
#define SQL_MAX_INDEX_SIZE                  102
#define SQL_MAX_ROW_SIZE_INCLUDES_LONG      103
#define SQL_MAX_ROW_SIZE                    104
#define SQL_MAX_STATEMENT_LEN               105
#define SQL_MAX_TABLES_IN_SELECT            106
#define SQL_MAX_USER_NAME_LEN               107
#define SQL_MAX_CHAR_LITERAL_LEN            108
#define SQL_TIMEDATE_ADD_INTERVALS          109
#define SQL_TIMEDATE_DIFF_INTERVALS         110
#define SQL_NEED_LONG_DATA_LEN              111
#define SQL_MAX_BINARY_LITERAL_LEN          112
#define SQL_LIKE_ESCAPE_CLAUSE              113
#define SQL_QUALIFIER_LOCATION              114

#if (ODBCVER >= 0x0201)
/*** ODBC SDK 2.01 Additions ***/
#define SQL_OJ_CAPABILITIES         65003  /* Temp value until ODBC 3.0 */
#endif  /* ODBCVER >= 0x0201 */

#define SQL_INFO_LAST                       SQL_QUALIFIER_LOCATION
#else
#define SQL_INFO_LAST                       SQL_NON_NULLABLE_COLUMNS
#endif  /* ODBCVER >= 0x0200 */

#define SQL_INFO_DRIVER_START               1000

/* SQL_CONVERT_*  return value bitmasks */

#define SQL_CVT_CHAR                        0x00000001L
#define SQL_CVT_NUMERIC                     0x00000002L
#define SQL_CVT_DECIMAL                     0x00000004L
#define SQL_CVT_INTEGER                     0x00000008L
#define SQL_CVT_SMALLINT                    0x00000010L
#define SQL_CVT_FLOAT                       0x00000020L
#define SQL_CVT_REAL                        0x00000040L
#define SQL_CVT_DOUBLE                      0x00000080L
#define SQL_CVT_VARCHAR                     0x00000100L
#define SQL_CVT_LONGVARCHAR                 0x00000200L
#define SQL_CVT_BINARY                      0x00000400L
#define SQL_CVT_VARBINARY                   0x00000800L
#define SQL_CVT_BIT                         0x00001000L
#define SQL_CVT_TINYINT                     0x00002000L
#define SQL_CVT_BIGINT                      0x00004000L
#define SQL_CVT_DATE                        0x00008000L
#define SQL_CVT_TIME                        0x00010000L
#define SQL_CVT_TIMESTAMP                   0x00020000L
#define SQL_CVT_LONGVARBINARY               0x00040000L

/* SQL_CONVERT_FUNCTIONS functions */
#define SQL_FN_CVT_CONVERT                  0x00000001L

/* SQL_STRING_FUNCTIONS functions */

#define SQL_FN_STR_CONCAT                   0x00000001L
#define SQL_FN_STR_INSERT                   0x00000002L
#define SQL_FN_STR_LEFT                     0x00000004L
#define SQL_FN_STR_LTRIM                    0x00000008L
#define SQL_FN_STR_LENGTH                   0x00000010L
#define SQL_FN_STR_LOCATE                   0x00000020L
#define SQL_FN_STR_LCASE                    0x00000040L
#define SQL_FN_STR_REPEAT                   0x00000080L
#define SQL_FN_STR_REPLACE                  0x00000100L
#define SQL_FN_STR_RIGHT                    0x00000200L
#define SQL_FN_STR_RTRIM                    0x00000400L
#define SQL_FN_STR_SUBSTRING                0x00000800L
#define SQL_FN_STR_UCASE                    0x00001000L
#define SQL_FN_STR_ASCII                    0x00002000L
#define SQL_FN_STR_CHAR                     0x00004000L
#if (ODBCVER >= 0x0200)
#define SQL_FN_STR_DIFFERENCE               0x00008000L
#define SQL_FN_STR_LOCATE_2                 0x00010000L
#define SQL_FN_STR_SOUNDEX                  0x00020000L
#define SQL_FN_STR_SPACE                    0x00040000L
#endif  /* ODBCVER >= 0x0200 */

/* SQL_NUMERIC_FUNCTIONS functions */

#define SQL_FN_NUM_ABS                      0x00000001L
#define SQL_FN_NUM_ACOS                     0x00000002L
#define SQL_FN_NUM_ASIN                     0x00000004L
#define SQL_FN_NUM_ATAN                     0x00000008L
#define SQL_FN_NUM_ATAN2                    0x00000010L
#define SQL_FN_NUM_CEILING                  0x00000020L
#define SQL_FN_NUM_COS                      0x00000040L
#define SQL_FN_NUM_COT                      0x00000080L
#define SQL_FN_NUM_EXP                      0x00000100L
#define SQL_FN_NUM_FLOOR                    0x00000200L
#define SQL_FN_NUM_LOG                      0x00000400L
#define SQL_FN_NUM_MOD                      0x00000800L
#define SQL_FN_NUM_SIGN                     0x00001000L
#define SQL_FN_NUM_SIN                      0x00002000L
#define SQL_FN_NUM_SQRT                     0x00004000L
#define SQL_FN_NUM_TAN                      0x00008000L
#define SQL_FN_NUM_PI                       0x00010000L
#define SQL_FN_NUM_RAND                     0x00020000L
#if (ODBCVER >= 0x0200)
#define SQL_FN_NUM_DEGREES                  0x00040000L
#define SQL_FN_NUM_LOG10                    0x00080000L
#define SQL_FN_NUM_POWER                    0x00100000L
#define SQL_FN_NUM_RADIANS                  0x00200000L
#define SQL_FN_NUM_ROUND                    0x00400000L
#define SQL_FN_NUM_TRUNCATE                 0x00800000L
#endif  /* ODBCVER >= 0x0200 */

/* SQL_TIMEDATE_FUNCTIONS functions */

#define SQL_FN_TD_NOW                       0x00000001L
#define SQL_FN_TD_CURDATE                   0x00000002L
#define SQL_FN_TD_DAYOFMONTH                0x00000004L
#define SQL_FN_TD_DAYOFWEEK                 0x00000008L
#define SQL_FN_TD_DAYOFYEAR                 0x00000010L
#define SQL_FN_TD_MONTH                     0x00000020L
#define SQL_FN_TD_QUARTER                   0x00000040L
#define SQL_FN_TD_WEEK                      0x00000080L
#define SQL_FN_TD_YEAR                      0x00000100L
#define SQL_FN_TD_CURTIME                   0x00000200L
#define SQL_FN_TD_HOUR                      0x00000400L
#define SQL_FN_TD_MINUTE                    0x00000800L
#define SQL_FN_TD_SECOND                    0x00001000L
#if (ODBCVER >= 0x0200)
#define SQL_FN_TD_TIMESTAMPADD              0x00002000L
#define SQL_FN_TD_TIMESTAMPDIFF             0x00004000L
#define SQL_FN_TD_DAYNAME                   0x00008000L
#define SQL_FN_TD_MONTHNAME                 0x00010000L
#endif  /* ODBCVER >= 0x0200 */

/* SQL_SYSTEM_FUNCTIONS functions */

#define SQL_FN_SYS_USERNAME                 0x00000001L
#define SQL_FN_SYS_DBNAME                   0x00000002L
#define SQL_FN_SYS_IFNULL                   0x00000004L

/* SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions */

#if (ODBCVER >= 0x0200)
#define SQL_FN_TSI_FRAC_SECOND              0x00000001L
#define SQL_FN_TSI_SECOND                   0x00000002L
#define SQL_FN_TSI_MINUTE                   0x00000004L
#define SQL_FN_TSI_HOUR                     0x00000008L
#define SQL_FN_TSI_DAY                      0x00000010L
#define SQL_FN_TSI_WEEK                     0x00000020L
#define SQL_FN_TSI_MONTH                    0x00000040L
#define SQL_FN_TSI_QUARTER                  0x00000080L
#define SQL_FN_TSI_YEAR                     0x00000100L
#endif  /* ODBCVER >= 0x0200 */

/* SQL_ODBC_API_CONFORMANCE values */

#define SQL_OAC_NONE                        0x0000
#define SQL_OAC_LEVEL1                      0x0001
#define SQL_OAC_LEVEL2                      0x0002

/* SQL_ODBC_SAG_CLI_CONFORMANCE values */

#define SQL_OSCC_NOT_COMPLIANT              0x0000
#define SQL_OSCC_COMPLIANT                  0x0001

/* SQL_ODBC_SQL_CONFORMANCE values */

#define SQL_OSC_MINIMUM                     0x0000
#define SQL_OSC_CORE                        0x0001
#define SQL_OSC_EXTENDED                    0x0002

/* SQL_CONCAT_NULL_BEHAVIOR values */

#define SQL_CB_NULL                         0x0000
#define SQL_CB_NON_NULL                     0x0001

/* SQL_CURSOR_COMMIT_BEHAVIOR and SQL_CURSOR_ROLLBACK_BEHAVIOR values */

#define SQL_CB_DELETE                       0x0000
#define SQL_CB_CLOSE                        0x0001
#define SQL_CB_PRESERVE                     0x0002

/* SQL_IDENTIFIER_CASE values */

#define SQL_IC_UPPER                        0x0001
#define SQL_IC_LOWER                        0x0002
#define SQL_IC_SENSITIVE                    0x0003
#define SQL_IC_MIXED                        0x0004

/* SQL_TXN_CAPABLE values */

#define SQL_TC_NONE                         0x0000
#define SQL_TC_DML                          0x0001
#define SQL_TC_ALL                          0x0002
#if (ODBCVER >= 0x0200)
#define SQL_TC_DDL_COMMIT                   0x0003
#define SQL_TC_DDL_IGNORE                   0x0004
#endif  /* ODBCVER >= 0x0200 */

/* SQL_SCROLL_OPTIONS masks */

#define SQL_SO_FORWARD_ONLY                 0x00000001L
#define SQL_SO_KEYSET_DRIVEN                0x00000002L
#define SQL_SO_DYNAMIC                      0x00000004L
#define SQL_SO_MIXED                        0x00000008L
#if (ODBCVER >= 0x0200)
#define SQL_SO_STATIC                       0x00000010L
#endif  /* ODBCVER >= 0x0200 */

/* SQL_SCROLL_CONCURRENCY masks */

#define SQL_SCCO_READ_ONLY                  0x00000001L
#define SQL_SCCO_LOCK                       0x00000002L
#define SQL_SCCO_OPT_ROWVER                 0x00000004L
#define SQL_SCCO_OPT_VALUES                 0x00000008L

/* SQL_FETCH_DIRECTION masks */

#define SQL_FD_FETCH_NEXT                   0x00000001L
#define SQL_FD_FETCH_FIRST                  0x00000002L
#define SQL_FD_FETCH_LAST                   0x00000004L
#define SQL_FD_FETCH_PRIOR                  0x00000008L
#define SQL_FD_FETCH_ABSOLUTE               0x00000010L
#define SQL_FD_FETCH_RELATIVE               0x00000020L
#define SQL_FD_FETCH_RESUME                 0x00000040L
#if (ODBCVER >= 0x0200)
#define SQL_FD_FETCH_BOOKMARK               0x00000080L
#endif  /* ODBCVER >= 0x0200 */

/* SQL_TXN_ISOLATION_OPTION masks */

#define SQL_TXN_READ_UNCOMMITTED            0x00000001L
#define SQL_TXN_READ_COMMITTED              0x00000002L
#define SQL_TXN_REPEATABLE_READ             0x00000004L
#define SQL_TXN_SERIALIZABLE                0x00000008L
#define SQL_TXN_VERSIONING                  0x00000010L

/* SQL_CORRELATION_NAME values */

#define SQL_CN_NONE                         0x0000
#define SQL_CN_DIFFERENT                    0x0001
#define SQL_CN_ANY                          0x0002

/* SQL_NON_NULLABLE_COLUMNS values */

#define SQL_NNC_NULL                        0x0000
#define SQL_NNC_NON_NULL                    0x0001

#if (ODBCVER >= 0x0200)
/* SQL_NULL_COLLATION values */

#define SQL_NC_HIGH                         0x0000
#define SQL_NC_LOW                          0x0001
#define SQL_NC_START                        0x0002
#define SQL_NC_END                          0x0004

/* SQL_FILE_USAGE values */

#define SQL_FILE_NOT_SUPPORTED              0x0000
#define SQL_FILE_TABLE                      0x0001
#define SQL_FILE_QUALIFIER                  0x0002

/* SQL_GETDATA_EXTENSIONS values */

#define SQL_GD_ANY_COLUMN                   0x00000001L
#define SQL_GD_ANY_ORDER                    0x00000002L
#define SQL_GD_BLOCK                        0x00000004L
#define SQL_GD_BOUND                        0x00000008L

/* SQL_ALTER_TABLE values */

#define SQL_AT_ADD_COLUMN                   0x00000001L
#define SQL_AT_DROP_COLUMN                  0x00000002L

/* SQL_POSITIONED_STATEMENTS masks */

#define SQL_PS_POSITIONED_DELETE            0x00000001L
#define SQL_PS_POSITIONED_UPDATE            0x00000002L
#define SQL_PS_SELECT_FOR_UPDATE            0x00000004L

/* SQL_GROUP_BY values */

#define SQL_GB_NOT_SUPPORTED                0x0000
#define SQL_GB_GROUP_BY_EQUALS_SELECT       0x0001
#define SQL_GB_GROUP_BY_CONTAINS_SELECT     0x0002
#define SQL_GB_NO_RELATION                  0x0003

/* SQL_OWNER_USAGE masks */

#define SQL_OU_DML_STATEMENTS               0x00000001L
#define SQL_OU_PROCEDURE_INVOCATION         0x00000002L
#define SQL_OU_TABLE_DEFINITION             0x00000004L
#define SQL_OU_INDEX_DEFINITION             0x00000008L
#define SQL_OU_PRIVILEGE_DEFINITION         0x00000010L

/* SQL_QUALIFIER_USAGE masks */

#define SQL_QU_DML_STATEMENTS               0x00000001L
#define SQL_QU_PROCEDURE_INVOCATION         0x00000002L
#define SQL_QU_TABLE_DEFINITION             0x00000004L
#define SQL_QU_INDEX_DEFINITION             0x00000008L
#define SQL_QU_PRIVILEGE_DEFINITION         0x00000010L

/* SQL_SUBQUERIES masks */

#define SQL_SQ_COMPARISON                   0x00000001L
#define SQL_SQ_EXISTS                       0x00000002L
#define SQL_SQ_IN                           0x00000004L
#define SQL_SQ_QUANTIFIED                   0x00000008L
#define SQL_SQ_CORRELATED_SUBQUERIES        0x00000010L

/* SQL_UNION masks */

#define SQL_U_UNION                         0x00000001L
#define SQL_U_UNION_ALL                     0x00000002L

/* SQL_BOOKMARK_PERSISTENCE values */

#define SQL_BP_CLOSE                        0x00000001L
#define SQL_BP_DELETE                       0x00000002L
#define SQL_BP_DROP                         0x00000004L
#define SQL_BP_TRANSACTION                  0x00000008L
#define SQL_BP_UPDATE                       0x00000010L
#define SQL_BP_OTHER_HSTMT                  0x00000020L
#define SQL_BP_SCROLL                       0x00000040L

/* SQL_STATIC_SENSITIVITY values */

#define SQL_SS_ADDITIONS                    0x00000001L
#define SQL_SS_DELETIONS                    0x00000002L
#define SQL_SS_UPDATES                      0x00000004L

/* SQL_LOCK_TYPESL masks */

#define SQL_LCK_NO_CHANGE                   0x00000001L
#define SQL_LCK_EXCLUSIVE                   0x00000002L
#define SQL_LCK_UNLOCK                      0x00000004L

/* SQL_POS_OPERATIONS masks */

#define SQL_POS_POSITION                    0x00000001L
#define SQL_POS_REFRESH                     0x00000002L
#define SQL_POS_UPDATE                      0x00000004L
#define SQL_POS_DELETE                      0x00000008L
#define SQL_POS_ADD                         0x00000010L

/* SQL_QUALIFIER_LOCATION values */

#define SQL_QL_START                        0x0001L
#define SQL_QL_END                          0x0002L

/* SQL_OJ_CAPABILITIES values */

#if (ODBCVER >= 0x0201)
#define SQL_OJ_LEFT                         0x00000001L
#define SQL_OJ_RIGHT                        0x00000002L
#define SQL_OJ_FULL                         0x00000004L
#define SQL_OJ_NESTED                       0x00000008L
#define SQL_OJ_NOT_ORDERED                  0x00000010L
#define SQL_OJ_INNER                        0x00000020L
#define SQL_OJ_ALL_COMPARISON_OPS           0x00000040L
#endif  /* ODBCVER >= 0x0201 */
#endif  /* ODBCVER >= 0x0200 */

/* options for SQLGetStmtOption/SQLSetStmtOption */
#define SQL_QUERY_TIMEOUT               0
#define SQL_MAX_ROWS                    1
#define SQL_NOSCAN                      2
#define SQL_MAX_LENGTH                  3
#define SQL_ASYNC_ENABLE                4
#define SQL_BIND_TYPE                   5
#if (ODBCVER >= 0x0200)
#define SQL_CURSOR_TYPE                 6
#define SQL_CONCURRENCY                 7
#define SQL_KEYSET_SIZE                 8
#define SQL_ROWSET_SIZE                 9
#define SQL_SIMULATE_CURSOR             10
#define SQL_RETRIEVE_DATA               11
#define SQL_USE_BOOKMARKS               12
#define SQL_GET_BOOKMARK                13      /*      GetStmtOption Only */
#define SQL_ROW_NUMBER                  14      /*      GetStmtOption Only */
#define SQL_STMT_OPT_MAX                SQL_ROW_NUMBER
#else
#define SQL_STMT_OPT_MAX                SQL_BIND_TYPE
#endif  /* ODBCVER >= 0x0200 */

#define SQL_STMT_OPT_MIN                SQL_QUERY_TIMEOUT


/* SQL_QUERY_TIMEOUT options */
#define SQL_QUERY_TIMEOUT_DEFAULT       0UL

/* SQL_MAX_ROWS options */
#define SQL_MAX_ROWS_DEFAULT            0UL

/* SQL_NOSCAN options */
#define SQL_NOSCAN_OFF                  0UL     /*      1.0 FALSE */
#define SQL_NOSCAN_ON                   1UL     /*      1.0 TRUE */
#define SQL_NOSCAN_DEFAULT              SQL_NOSCAN_OFF

/* SQL_MAX_LENGTH options */
#define SQL_MAX_LENGTH_DEFAULT          0UL

/* SQL_ASYNC_ENABLE options */
#define SQL_ASYNC_ENABLE_OFF            0UL
#define SQL_ASYNC_ENABLE_ON             1UL
#define SQL_ASYNC_ENABLE_DEFAULT        SQL_ASYNC_ENABLE_OFF

/* SQL_BIND_TYPE options */
#define SQL_BIND_BY_COLUMN              0UL
#define SQL_BIND_TYPE_DEFAULT           SQL_BIND_BY_COLUMN  /* Default value */

/* SQL_CONCURRENCY options */
#define SQL_CONCUR_READ_ONLY            1
#define SQL_CONCUR_LOCK                 2
#define SQL_CONCUR_ROWVER               3
#define SQL_CONCUR_VALUES               4
#define SQL_CONCUR_DEFAULT              SQL_CONCUR_READ_ONLY /* Default value
*/

#if (ODBCVER >= 0x0200)
/* SQL_CURSOR_TYPE options */
#define SQL_CURSOR_FORWARD_ONLY         0UL
#define SQL_CURSOR_KEYSET_DRIVEN        1UL
#define SQL_CURSOR_DYNAMIC              2UL
#define SQL_CURSOR_STATIC               3UL
#define SQL_CURSOR_TYPE_DEFAULT         SQL_CURSOR_FORWARD_ONLY /* Default value */

/* SQL_ROWSET_SIZE options */
#define SQL_ROWSET_SIZE_DEFAULT         1UL

/* SQL_KEYSET_SIZE options */
#define SQL_KEYSET_SIZE_DEFAULT         0UL

/* SQL_SIMULATE_CURSOR options */
#define SQL_SC_NON_UNIQUE               0UL
#define SQL_SC_TRY_UNIQUE               1UL
#define SQL_SC_UNIQUE                   2UL

/* SQL_RETRIEVE_DATA options */
#define SQL_RD_OFF                      0UL
#define SQL_RD_ON                       1UL
#define SQL_RD_DEFAULT                  SQL_RD_ON

/* SQL_USE_BOOKMARKS options */
#define SQL_UB_OFF                      0UL
#define SQL_UB_ON                       1UL
#define SQL_UB_DEFAULT                  SQL_UB_OFF

#endif  /* ODBCVER >= 0x0200 */

/* options for SQLSetConnectOption/SQLGetConnectOption */
#define SQL_ACCESS_MODE                 101
#define SQL_AUTOCOMMIT                  102
#define SQL_LOGIN_TIMEOUT               103
#define SQL_OPT_TRACE                   104
#define SQL_OPT_TRACEFILE               105
#define SQL_TRANSLATE_DLL               106
#define SQL_TRANSLATE_OPTION            107
#define SQL_TXN_ISOLATION               108
#define SQL_CURRENT_QUALIFIER           109
#if (ODBCVER >= 0x0200)
#define SQL_ODBC_CURSORS                110
#define SQL_QUIET_MODE                  111
#define SQL_PACKET_SIZE                 112
#define SQL_CONN_OPT_MAX                SQL_PACKET_SIZE
#else
#define SQL_CONN_OPT_MAX                SQL_CURRENT_QUALIFIER
#endif  /* ODBCVER >= 0x0200 */
#define SQL_CONNECT_OPT_DRVR_START      1000

#define SQL_CONN_OPT_MIN                SQL_ACCESS_MODE

/* SQL_ACCESS_MODE options */
#define SQL_MODE_READ_WRITE             0UL
#define SQL_MODE_READ_ONLY              1UL
#define SQL_MODE_DEFAULT                SQL_MODE_READ_WRITE

/* SQL_AUTOCOMMIT options */
#define SQL_AUTOCOMMIT_OFF              0UL
#define SQL_AUTOCOMMIT_ON               1UL
#define SQL_AUTOCOMMIT_DEFAULT          SQL_AUTOCOMMIT_ON

/* SQL_LOGIN_TIMEOUT options */
#define SQL_LOGIN_TIMEOUT_DEFAULT       15UL

/* SQL_OPT_TRACE options */
#define SQL_OPT_TRACE_OFF               0UL
#define SQL_OPT_TRACE_ON                1UL
#define SQL_OPT_TRACE_DEFAULT           SQL_OPT_TRACE_OFF
#define SQL_OPT_TRACE_FILE_DEFAULT      "\\SQL.LOG"

#if (ODBCVER >= 0x0200)
/* SQL_ODBC_CURSORS options */
#define SQL_CUR_USE_IF_NEEDED           0UL
#define SQL_CUR_USE_ODBC                1UL
#define SQL_CUR_USE_DRIVER              2UL
#define SQL_CUR_DEFAULT                 SQL_CUR_USE_DRIVER
#endif  /* ODBCVER >= 0x0200 */

/* Column types and scopes in SQLSpecialColumns.  */
#define SQL_BEST_ROWID                  1
#define SQL_ROWVER                      2

#define SQL_SCOPE_CURROW                0
#define SQL_SCOPE_TRANSACTION           1
#define SQL_SCOPE_SESSION               2

/* Defines for SQLStatistics */
#define SQL_INDEX_UNIQUE                0
#define SQL_INDEX_ALL                   1

#define SQL_QUICK                       0
#define SQL_ENSURE                      1

/* Defines for SQLStatistics (returned in the result set) */
#define SQL_TABLE_STAT                  0
#define SQL_INDEX_CLUSTERED             1
#define SQL_INDEX_HASHED                2
#define SQL_INDEX_OTHER                 3

#if (ODBCVER >= 0x0200)
/* Defines for SQLSpecialColumns (returned in the result set) */
#define SQL_PC_UNKNOWN                  0
#define SQL_PC_NOT_PSEUDO               1
#define SQL_PC_PSEUDO                   2
#endif  /* ODBCVER >= 0x0200 */

/* SQLDataSources "fDirection" values, also used on SQLExtendedFetch() */
#define SQL_FETCH_NEXT					1
#define SQL_FETCH_FIRST					2

#ifndef RC_INVOKED

#if (ODBCVER >= 0x0200)
/*      This define is too large for RC */
#define SQL_ODBC_KEYWORDS \
"ABSOLUTE,ACTION,ADA,ADD,ALL,ALLOCATE,ALTER,AND,ANY,ARE,AS,"\
"ASC,ASSERTION,AT,AUTHORIZATION,AVG,"\
"BEGIN,BETWEEN,BIT,BIT_LENGTH,BOTH,BY,CASCADE,CASCADED,CASE,CAST,CATALOG,"\
"CHAR,CHAR_LENGTH,CHARACTER,CHARACTER_LENGTH,CHECK,CLOSE,COALESCE,"\
"COBOL,COLLATE,COLLATION,COLUMN,COMMIT,CONNECT,CONNECTION,CONSTRAINT,"\
"CONSTRAINTS,CONTINUE,CONVERT,CORRESPONDING,COUNT,CREATE,CROSS,CURRENT,"\
"CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,"\
"DATE,DAY,DEALLOCATE,DEC,DECIMAL,DECLARE,DEFAULT,DEFERRABLE,"\
"DEFERRED,DELETE,DESC,DESCRIBE,DESCRIPTOR,DIAGNOSTICS,DISCONNECT,"\
"DISTINCT,DOMAIN,DOUBLE,DROP,"\
"ELSE,END,END-EXEC,ESCAPE,EXCEPT,EXCEPTION,EXEC,EXECUTE,"\
"EXISTS,EXTERNAL,EXTRACT,"\
"FALSE,FETCH,FIRST,FLOAT,FOR,FOREIGN,FORTRAN,FOUND,FROM,FULL,"\
"GET,GLOBAL,GO,GOTO,GRANT,GROUP,HAVING,HOUR,"\
"IDENTITY,IMMEDIATE,IN,INCLUDE,INDEX,INDICATOR,INITIALLY,INNER,"\
"INPUT,INSENSITIVE,INSERT,INTEGER,INTERSECT,INTERVAL,INTO,IS,ISOLATION,"\
"JOIN,KEY,LANGUAGE,LAST,LEADING,LEFT,LEVEL,LIKE,LOCAL,LOWER,"\
"MATCH,MAX,MIN,MINUTE,MODULE,MONTH,MUMPS,"\
"NAMES,NATIONAL,NATURAL,NCHAR,NEXT,NO,NONE,NOT,NULL,NULLIF,NUMERIC,"\
"OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,ORDER,OUTER,OUTPUT,OVERLAPS,"\
"PAD,PARTIAL,PASCAL,PLI,POSITION,PRECISION,PREPARE,PRESERVE,"\
"PRIMARY,PRIOR,PRIVILEGES,PROCEDURE,PUBLIC,"\
"REFERENCES,RELATIVE,RESTRICT,REVOKE,RIGHT,ROLLBACK,ROWS,"\
"SCHEMA,SCROLL,SECOND,SECTION,SELECT,SEQUENCE,SESSION,SESSION_USER,SET,SIZE,"\
"SMALLINT,SOME,SPACE,SQL,SQLCA,SQLCODE,SQLERROR,SQLSTATE,SQLWARNING,"\
"SUBSTRING,SUM,SYSTEM_USER,"\
"TABLE,TEMPORARY,THEN,TIME,TIMESTAMP,TIMEZONE_HOUR,TIMEZONE_MINUTE,"\
"TO,TRAILING,TRANSACTION,TRANSLATE,TRANSLATION,TRIM,TRUE,"\
"UNION,UNIQUE,UNKNOWN,UPDATE,UPPER,USAGE,USER,USING,"\
"VALUE,VALUES,VARCHAR,VARYING,VIEW,WHEN,WHENEVER,WHERE,WITH,WORK,YEAR"
#endif  /* ODBCVER >= 0x0200 */

/* Level 1 Prototypes */
SQLRETURN SQL_API SQLColumns(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szTableName,
    SQLSMALLINT        cbTableName,
    SQLCHAR FAR       *szColumnName,
    SQLSMALLINT        cbColumnName);

SQLRETURN SQL_API SQLGetConnectOption(
    SQLHDBC            hdbc,
    SQLUSMALLINT       fOption,
    SQLPOINTER         pvParam);

SQLRETURN SQL_API SQLGetData(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       icol,
    SQLSMALLINT        fCType,
    SQLPOINTER         rgbValue,
    SQLINTEGER         cbValueMax,
    SQLINTEGER FAR    *pcbValue);

SQLRETURN SQL_API SQLGetFunctions(
    SQLHDBC            hdbc,
    SQLUSMALLINT       fFunction,
    SQLUSMALLINT FAR  *pfExists);

SQLRETURN SQL_API SQLGetInfo(
    SQLHDBC            hdbc,
    SQLUSMALLINT       fInfoType,
    SQLPOINTER         rgbInfoValue,
    SQLSMALLINT        cbInfoValueMax,
    SQLSMALLINT FAR   *pcbInfoValue);

SQLRETURN SQL_API SQLGetStmtOption(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       fOption,
    SQLPOINTER         pvParam);

SQLRETURN SQL_API SQLGetTypeInfo(
    SQLHSTMT           hstmt,
    SQLSMALLINT        fSqlType);

SQLRETURN SQL_API SQLParamData(
    SQLHSTMT           hstmt,
    SQLPOINTER FAR    *prgbValue);

SQLRETURN SQL_API SQLPutData(
    SQLHSTMT           hstmt,
    SQLPOINTER         rgbValue,
    SQLINTEGER         cbValue);

SQLRETURN SQL_API SQLSetConnectOption(
    SQLHDBC            hdbc,
    SQLUSMALLINT       fOption,
    SQLUINTEGER        vParam);

SQLRETURN SQL_API SQLSetStmtOption(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       fOption,
    SQLUINTEGER        vParam);

SQLRETURN SQL_API SQLSpecialColumns(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       fColType,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szTableName,
    SQLSMALLINT        cbTableName,
    SQLUSMALLINT       fScope,
    SQLUSMALLINT       fNullable);

SQLRETURN SQL_API SQLStatistics(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szTableName,
    SQLSMALLINT        cbTableName,
    SQLUSMALLINT       fUnique,
    SQLUSMALLINT       fAccuracy);

SQLRETURN SQL_API SQLTables(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szTableName,
    SQLSMALLINT        cbTableName,
    SQLCHAR FAR       *szTableType,
    SQLSMALLINT        cbTableType);
#endif /* RC_INVOKED */


/* Level 2 Functions                             */



#ifndef RC_INVOKED

SQLRETURN SQL_API SQLDataSources(
    SQLHENV            henv,
    SQLUSMALLINT       fDirection,
    SQLCHAR FAR       *szDSN,
    SQLSMALLINT        cbDSNMax,
    SQLSMALLINT FAR   *pcbDSN,
    SQLCHAR FAR       *szDescription,
    SQLSMALLINT        cbDescriptionMax,
    SQLSMALLINT FAR   *pcbDescription);


#endif /* RC_INVOKED */

/*      Deprecated defines from prior versions of ODBC */
#define SQL_DATABASE_NAME               16    /* Use SQLGetConnectOption/SQL_CURRENT_QUALIFIER */
#define SQL_FD_FETCH_PREV               SQL_FD_FETCH_PRIOR
#define SQL_FETCH_PREV                  SQL_FETCH_PRIOR
#define SQL_CONCUR_TIMESTAMP            SQL_CONCUR_ROWVER
#define SQL_SCCO_OPT_TIMESTAMP          SQL_SCCO_OPT_ROWVER
#define SQL_CC_DELETE                   SQL_CB_DELETE
#define SQL_CR_DELETE                   SQL_CB_DELETE
#define SQL_CC_CLOSE                    SQL_CB_CLOSE
#define SQL_CR_CLOSE                    SQL_CB_CLOSE
#define SQL_CC_PRESERVE                 SQL_CB_PRESERVE
#define SQL_CR_PRESERVE                 SQL_CB_PRESERVE
#define SQL_FETCH_RESUME                7     /* Not supported by 2.0 drivers */
#define SQL_SCROLL_FORWARD_ONLY         0L    /*-SQL_CURSOR_FORWARD_ONLY */
#define SQL_SCROLL_KEYSET_DRIVEN        (-1L) /*-SQL_CURSOR_KEYSET_DRIVEN */
#define SQL_SCROLL_DYNAMIC              (-2L) /*-SQL_CURSOR_DYNAMIC */
#if (ODBCVER >= 0x0200)
#define SQL_SCROLL_STATIC               (-3L) /*-SQL_CURSOR_STATIC */
#define SQL_PC_NON_PSEUDO               SQL_PC_NOT_PSEUDO
#endif  /* ODBCVER >= 0x0200 */

/*      Deprecrated functions from prior versions of ODBC */
#ifndef RC_INVOKED

SQLRETURN SQL_API SQLSetScrollOptions(    /*      Use SQLSetStmtOptions */
    SQLHSTMT           hstmt,
    SQLUSMALLINT       fConcurrency,
    SQLINTEGER         crowKeyset,
    SQLUSMALLINT       crowRowset);

#endif /* RC_INVOKED */

#ifdef __cplusplus
}                                    /* End of extern "C" { */
#endif  /* __cplusplus */

#endif  /* #ifndef __SQL */
