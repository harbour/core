/*****************************************************************
** SQLEXT.H - This is the include for applications using
**             the Microsoft SQL Extensions
**
** (C) Copyright 1990 - 1995 By Microsoft Corp.
**
**      Updated 5/12/93 for 2.00 specification
**      Updated 5/23/94 for 2.01 specification
**      Updated 10/27/94 for 2.10 specification
**      Updated 04/10/95 for 2.50 specification
*********************************************************************/

#ifndef __SQLEXT
#define __SQLEXT

#ifndef __SQL
#include "sql.h"
#endif

#ifdef __cplusplus
extern "C" {                         /* Assume C declarations for C++ */
#endif  /* __cplusplus */

/* SQLBindParameter extensions */
#if (ODBCVER >= 0x0200)
#define SQL_DEFAULT_PARAM            (-5)
#define SQL_IGNORE                   (-6)
#define SQL_LEN_DATA_AT_EXEC_OFFSET  (-100)
#define SQL_LEN_DATA_AT_EXEC(length) (-length+SQL_LEN_DATA_AT_EXEC_OFFSET)
#endif  /* ODBCVER >= 0x0200 */


/* Defines for SQLSetPos */
#define SQL_ENTIRE_ROWSET            0

/* Operations in SQLSetPos */
#define SQL_POSITION                 0               /*      1.0 FALSE */
#define SQL_REFRESH                  1               /*      1.0 TRUE */
#if (ODBCVER >= 0x0200)
#define SQL_UPDATE                   2
#define SQL_DELETE                   3
#define SQL_ADD                      4
#endif  /* ODBCVER >= 0x0200 */

/* Lock options in SQLSetPos */
#define SQL_LOCK_NO_CHANGE           0               /*      1.0 FALSE */
#define SQL_LOCK_EXCLUSIVE           1               /*      1.0 TRUE */
#if (ODBCVER >= 0x0200)
#define SQL_LOCK_UNLOCK              2

/* Macros for SQLSetPos */
#define SQL_POSITION_TO(hstmt,irow) SQLSetPos(hstmt,irow,SQL_POSITION,SQL_LOCK_NO_CHANGE)
#define SQL_LOCK_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_POSITION,fLock)
#define SQL_REFRESH_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_REFRESH,fLock)
#define SQL_UPDATE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_UPDATE,SQL_LOCK_NO_CHANGE)
#define SQL_DELETE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_DELETE,SQL_LOCK_NO_CHANGE)
#define SQL_ADD_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_ADD,SQL_LOCK_NO_CHANGE)
#endif  /* ODBCVER >= 0x0200 */

/* Level 1 Prototypes                            */

/* Options for SQLDriverConnect */
#define SQL_DRIVER_NOPROMPT             0
#define SQL_DRIVER_COMPLETE             1
#define SQL_DRIVER_PROMPT               2
#define SQL_DRIVER_COMPLETE_REQUIRED    3

#ifndef RC_INVOKED

SQLRETURN SQL_API SQLDriverConnect(
    SQLHDBC            hdbc,
    SQLHWND            hwnd,
    SQLCHAR FAR       *szConnStrIn,
    SQLSMALLINT        cbConnStrIn,
    SQLCHAR FAR       *szConnStrOut,
    SQLSMALLINT        cbConnStrOutMax,
    SQLSMALLINT FAR   *pcbConnStrOut,
    SQLUSMALLINT       fDriverCompletion);

#endif /* RC_INVOKED */

/* Level 2 Functions                             */

/* SQLExtendedFetch "fFetchType" values */
#ifndef SQL_FETCH_NEXT
#define SQL_FETCH_NEXT                   1
#endif
#ifndef SQL_FETCH_FIRST
#define SQL_FETCH_FIRST                  2
#endif
#define SQL_FETCH_LAST                   3
#define SQL_FETCH_PRIOR                  4
#define SQL_FETCH_ABSOLUTE               5
#define SQL_FETCH_RELATIVE               6
#if (ODBCVER >= 0x0200)
#define SQL_FETCH_BOOKMARK               8
#endif  /* ODBCVER >= 0x0200 */

/* SQLExtendedFetch "rgfRowStatus" element values */
#define SQL_ROW_SUCCESS                  0
#define SQL_ROW_DELETED                  1
#define SQL_ROW_UPDATED                  2
#define SQL_ROW_NOROW                    3
#if (ODBCVER >= 0x0200)
#define SQL_ROW_ADDED                    4
#define SQL_ROW_ERROR                    5
#endif  /* ODBCVER >= 0x0200 */

/* Defines for SQLForeignKeys (returned in result set) */
#define SQL_CASCADE                      0
#define SQL_RESTRICT                     1
#define SQL_SET_NULL                     2
#if (ODBCVER >= 0x0250)
#define SQL_NO_ACTION			 3
#define SQL_SET_DEFAULT			 4
#endif

/* Defines for SQLBindParameter and
                           SQLProcedureColumns (returned in the result set) */
#define SQL_PARAM_TYPE_UNKNOWN           0
#define SQL_PARAM_INPUT                  1
#define SQL_PARAM_INPUT_OUTPUT           2
#define SQL_RESULT_COL                   3
#if (ODBCVER >= 0x0200)
#define SQL_PARAM_OUTPUT                 4
#define SQL_RETURN_VALUE                 5
#endif  /* ODBCVER >= 0x0200 */


#if (ODBCVER >= 0x0200)
/* Defines for SQLProcedures (returned in the result set) */
#define SQL_PT_UNKNOWN                   0
#define SQL_PT_PROCEDURE                 1
#define SQL_PT_FUNCTION                  2

#endif  /* ODBCVER >= 0x0200 */

/* Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter
*/
#define SQL_PARAM_TYPE_DEFAULT           SQL_PARAM_INPUT_OUTPUT
#define SQL_SETPARAM_VALUE_MAX           (-1L)


#ifndef RC_INVOKED

/* Level 2 Prototypes */
SQLRETURN SQL_API SQLBrowseConnect(
    SQLHDBC            hdbc,
    SQLCHAR FAR       *szConnStrIn,
    SQLSMALLINT        cbConnStrIn,
    SQLCHAR FAR       *szConnStrOut,
    SQLSMALLINT        cbConnStrOutMax,
    SQLSMALLINT FAR   *pcbConnStrOut);

SQLRETURN SQL_API SQLColumnPrivileges(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szTableName,
    SQLSMALLINT        cbTableName,
    SQLCHAR FAR       *szColumnName,
    SQLSMALLINT        cbColumnName);

SQLRETURN SQL_API SQLDescribeParam(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       ipar,
    SQLSMALLINT FAR   *pfSqlType,
    SQLUINTEGER FAR   *pcbParamDef,
    SQLSMALLINT FAR   *pibScale,
    SQLSMALLINT FAR   *pfNullable);

SQLRETURN SQL_API SQLExtendedFetch(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       fFetchType,
    SQLINTEGER         irow,
    SQLUINTEGER FAR   *pcrow,
    SQLUSMALLINT FAR  *rgfRowStatus);

SQLRETURN SQL_API SQLForeignKeys(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szPkCatalogName,
    SQLSMALLINT        cbPkCatalogName,
    SQLCHAR FAR       *szPkSchemaName,
    SQLSMALLINT        cbPkSchemaName,
    SQLCHAR FAR       *szPkTableName,
    SQLSMALLINT        cbPkTableName,
    SQLCHAR FAR       *szFkCatalogName,
    SQLSMALLINT        cbFkCatalogName,
    SQLCHAR FAR       *szFkSchemaName,
    SQLSMALLINT        cbFkSchemaName,
    SQLCHAR FAR       *szFkTableName,
    SQLSMALLINT        cbFkTableName);

SQLRETURN SQL_API SQLMoreResults(
    SQLHSTMT           hstmt);

SQLRETURN SQL_API SQLNativeSql(
    SQLHDBC            hdbc,
    SQLCHAR FAR       *szSqlStrIn,
    SQLINTEGER         cbSqlStrIn,
    SQLCHAR FAR       *szSqlStr,
    SQLINTEGER         cbSqlStrMax,
    SQLINTEGER FAR    *pcbSqlStr);

SQLRETURN SQL_API SQLNumParams(
    SQLHSTMT           hstmt,
    SQLSMALLINT FAR   *pcpar);

SQLRETURN SQL_API SQLParamOptions(
    SQLHSTMT           hstmt,
    SQLUINTEGER        crow,
    SQLUINTEGER FAR   *pirow);

SQLRETURN SQL_API SQLPrimaryKeys(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szTableName,
    SQLSMALLINT        cbTableName);

SQLRETURN SQL_API SQLProcedureColumns(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szProcName,
    SQLSMALLINT        cbProcName,
    SQLCHAR FAR       *szColumnName,
    SQLSMALLINT        cbColumnName);

SQLRETURN SQL_API SQLProcedures(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szProcName,
    SQLSMALLINT        cbProcName);

SQLRETURN SQL_API SQLSetPos(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       irow,
    SQLUSMALLINT       fOption,
    SQLUSMALLINT       fLock);

SQLRETURN SQL_API SQLTablePrivileges(
    SQLHSTMT           hstmt,
    SQLCHAR FAR       *szCatalogName,
    SQLSMALLINT        cbCatalogName,
    SQLCHAR FAR       *szSchemaName,
    SQLSMALLINT        cbSchemaName,
    SQLCHAR FAR       *szTableName,
    SQLSMALLINT        cbTableName);

/* SDK 2.0 Additions */

#if (ODBCVER >= 0x0200)
SQLRETURN SQL_API SQLDrivers(
    SQLHENV            henv,
    SQLUSMALLINT       fDirection,
    SQLCHAR FAR       *szDriverDesc,
    SQLSMALLINT        cbDriverDescMax,
    SQLSMALLINT FAR   *pcbDriverDesc,
    SQLCHAR FAR       *szDriverAttributes,
    SQLSMALLINT        cbDrvrAttrMax,
    SQLSMALLINT FAR   *pcbDrvrAttr);

SQLRETURN SQL_API SQLBindParameter(
    SQLHSTMT           hstmt,
    SQLUSMALLINT       ipar,
    SQLSMALLINT        fParamType,
    SQLSMALLINT        fCType,
    SQLSMALLINT        fSqlType,
    SQLUINTEGER        cbColDef,
    SQLSMALLINT        ibScale,
    SQLPOINTER         rgbValue,
    SQLINTEGER         cbValueMax,
    SQLINTEGER FAR    *pcbValue);
#endif  /* ODBCVER >= 0x0200 */

#endif /* RC_INVOKED */


#ifdef __cplusplus
}                                     /* End of extern "C" { */
#endif  /* __cplusplus */

#endif /* __SQLEXT */
