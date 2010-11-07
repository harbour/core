/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Headers for ODBC
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HBODBC_CH_
#define HBODBC_CH_

/* RETCODEs */
#define SQL_INVALID_HANDLE              ( -2 )
#define SQL_ERROR                       ( -1 )
#define SQL_SUCCESS                     0
#define SQL_SUCCESS_WITH_INFO           1
#define SQL_NO_DATA_FOUND               100
#define SQL_NEED_DATA                   99

/* Standard SQL datatypes, using ANSI type numbering */
#define SQL_CHAR                        1
#define SQL_NUMERIC                     2
#define SQL_DECIMAL                     3
#define SQL_INTEGER                     4
#define SQL_SMALLINT                    5
#define SQL_FLOAT                       6
#define SQL_REAL                        7
#define SQL_DOUBLE                      8
#define SQL_DATE                        9
#define SQL_TIME                        10
#define SQL_TIMESTAMP                   11
#define SQL_VARCHAR                     12
#define SQL_BIT                         ( -7 )
#define SQL_LONGVARCHAR                 ( -1 )
#define SQL_LONGVARBINARY               ( -4 )
#define SQL_BIGINT                      ( -5 )
#define SQL_TINYINT                     ( -6 )

#define SQL_NVARCHAR                    ( -9 )

#define SQL_TYPE_NULL                   0
#define SQL_TYPE_MIN                    SQL_BIT
#define SQL_TYPE_MAX                    SQL_VARCHAR
#define SQL_ALL_TYPES                   0

/* NULL status constants.  These are used in SQLColumns, SQLColAttributes,
SQLDescribeCol, SQLDescribeParam, and SQLSpecialColumns to describe the
nullablity of a column in a table. */
#define SQL_NO_NULLS                    0
#define SQL_NULLABLE                    1
#define SQL_NULLABLE_UNKNOWN            2

/* Special length values */
#define SQL_NULL_DATA                   ( -1 )
#define SQL_DATA_AT_EXEC                ( -2 )
#define SQL_NTS                         ( -3 )

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

#define SQL_FETCH_NEXT                  1
#define SQL_FETCH_FIRST                 2
#define SQL_FETCH_LAST                  3
#define SQL_FETCH_PRIOR                 4
#define SQL_FETCH_ABSOLUTE              5
#define SQL_FETCH_RELATIVE              6
#define SQL_FETCH_BOOKMARK              8

/* SQL Options */

#define SQL_AUTOCOMMIT_OFF              0
#define SQL_AUTOCOMMIT_ON               1
#define SQL_AUTOCOMMIT                  102

#define SQL_INFO_FIRST                  0
#define SQL_ACTIVE_CONNECTIONS          0
#define SQL_ACTIVE_STATEMENTS           1
#define SQL_DATA_SOURCE_NAME            2
#define SQL_DRIVER_HDBC                 3
#define SQL_DRIVER_HENV                 4
#define SQL_DRIVER_HSTMT                5
#define SQL_DRIVER_NAME                 6
#define SQL_DRIVER_VER                  7
#define SQL_FETCH_DIRECTION             8
#define SQL_ODBC_API_CONFORMANCE        9
#define SQL_ODBC_VER                    10
#define SQL_ROW_UPDATES                 11
#define SQL_ODBC_SAG_CLI_CONFORMANCE    12
#define SQL_SERVER_NAME                 13
#define SQL_SEARCH_PATTERN_ESCAPE       14
#define SQL_ODBC_SQL_CONFORMANCE        15

#define SQL_DATABASE_NAME               16
#define SQL_DBMS_NAME                   17
#define SQL_DBMS_VER                    18

#define SQL_ACCESSIBLE_TABLES           19
#define SQL_ACCESSIBLE_PROCEDURES       20
#define SQL_PROCEDURES                  21
#define SQL_CONCAT_NULL_BEHAVIOR        22
#define SQL_CURSOR_COMMIT_BEHAVIOR      23
#define SQL_CURSOR_ROLLBACK_BEHAVIOR    24
#define SQL_DATA_SOURCE_READ_ONLY       25
#define SQL_DEFAULT_TXN_ISOLATION       26
#define SQL_EXPRESSIONS_IN_ORDERBY      27
#define SQL_IDENTIFIER_CASE             28
#define SQL_IDENTIFIER_QUOTE_CHAR       29
#define SQL_MAX_COLUMN_NAME_LEN         30
#define SQL_MAX_CURSOR_NAME_LEN         31
#define SQL_MAX_OWNER_NAME_LEN          32
#define SQL_MAX_PROCEDURE_NAME_LEN      33
#define SQL_MAX_QUALIFIER_NAME_LEN      34
#define SQL_MAX_TABLE_NAME_LEN          35
#define SQL_MULT_RESULT_SETS            36
#define SQL_MULTIPLE_ACTIVE_TXN         37
#define SQL_OUTER_JOINS                 38
#define SQL_OWNER_TERM                  39
#define SQL_PROCEDURE_TERM              40
#define SQL_QUALIFIER_NAME_SEPARATOR    41
#define SQL_QUALIFIER_TERM              42
#define SQL_SCROLL_CONCURRENCY          43
#define SQL_SCROLL_OPTIONS              44
#define SQL_TABLE_TERM                  45
#define SQL_TXN_CAPABLE                 46
#define SQL_USER_NAME                   47

#define SQL_CONVERT_FUNCTIONS           48
#define SQL_NUMERIC_FUNCTIONS           49
#define SQL_STRING_FUNCTIONS            50
#define SQL_SYSTEM_FUNCTIONS            51
#define SQL_TIMEDATE_FUNCTIONS          52

#define SQL_CONVERT_BIGINT              53
#define SQL_CONVERT_BINARY              54
#define SQL_CONVERT_BIT                 55
#define SQL_CONVERT_CHAR                56
#define SQL_CONVERT_DATE                57
#define SQL_CONVERT_DECIMAL             58
#define SQL_CONVERT_DOUBLE              59
#define SQL_CONVERT_FLOAT               60
#define SQL_CONVERT_INTEGER             61
#define SQL_CONVERT_LONGVARCHAR         62
#define SQL_CONVERT_NUMERIC             63
#define SQL_CONVERT_REAL                64
#define SQL_CONVERT_SMALLINT            65
#define SQL_CONVERT_TIME                66
#define SQL_CONVERT_TIMESTAMP           67
#define SQL_CONVERT_TINYINT             68
#define SQL_CONVERT_VARBINARY           69
#define SQL_CONVERT_VARCHAR             70
#define SQL_CONVERT_LONGVARBINARY       71

#define SQL_TXN_ISOLATION_OPTION        72
#define SQL_ODBC_SQL_OPT_IEF            73

#define SQL_QUERY_TIMEOUT               0
#define SQL_MAX_ROWS                    1
#define SQL_NOSCAN                      2
#define SQL_MAX_LENGTH                  3
#define SQL_ASYNC_ENABLE                4
#define SQL_BIND_TYPE                   5

#define SQL_HANDLE_ENV                  1
#define SQL_HANDLE_DBC                  2
#define SQL_HANDLE_STMT                 3
#define SQL_HANDLE_DESC                 4

#endif
