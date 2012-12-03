/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SQLite3 library low level (client api) interface code
 *
 * Copyright 2007 P.Chornyj <myorg63@mail.ru>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HBSQLIT3_CH_
#define HBSQLIT3_CH_

#xtranslate DB_IS_OPEN( <db> )         => ( ! Empty( <db> ) )
#xtranslate STMT_IS_PREPARED( <stmt> ) => ( ! Empty( <stmt> ) )

/* Fundamental Datatypes */
#define SQLITE_INTEGER                     1
#define SQLITE_FLOAT                       2
#define SQLITE_TEXT                        3
#define SQLITE3_TEXT                       SQLITE_TEXT
#define SQLITE_BLOB                        4
#define SQLITE_NULL                        5
                                           
#define SQLITE_OK                          0   /* Successful result */
/* Beginning-of-Error-Codes */             
#define SQLITE_ERROR                       1   /* SQL error or missing database */
#define SQLITE_INTERNAL                    2   /* NOT USED. Internal logic error in SQLite */
#define SQLITE_PERM                        3   /* Access permission denied */
#define SQLITE_ABORT                       4   /* Callback routine requested an abort */
#define SQLITE_BUSY                        5   /* The database file is locked */
#define SQLITE_LOCKED                      6   /* A table in the database is locked */
#define SQLITE_NOMEM                       7   /* A malloc() failed */
#define SQLITE_READONLY                    8   /* Attempt to write a readonly database */
#define SQLITE_INTERRUPT                   9   /* Operation terminated by sqlite3_interrupt()*/
#define SQLITE_IOERR                       10  /* Some kind of disk I/O error occurred */
#define SQLITE_CORRUPT                     11  /* The database disk image is malformed */
#define SQLITE_NOTFOUND                    12  /* NOT USED. Table or record not found */
#define SQLITE_FULL                        13  /* Insertion failed because database is full */
#define SQLITE_CANTOPEN                    14  /* Unable to open the database file */
#define SQLITE_PROTOCOL                    15  /* NOT USED. Database lock protocol error */
#define SQLITE_EMPTY                       16  /* Database is empty */
#define SQLITE_SCHEMA                      17  /* The database schema changed */
#define SQLITE_TOOBIG                      18  /* String or BLOB exceeds size limit */
#define SQLITE_CONSTRAINT                  19  /* Abort due to constraint violation */
#define SQLITE_MISMATCH                    20  /* Data type mismatch */
#define SQLITE_MISUSE                      21  /* Library used incorrectly */
#define SQLITE_NOLFS                       22  /* Uses OS features not supported on host */
#define SQLITE_AUTH                        23  /* Authorization denied */
#define SQLITE_FORMAT                      24  /* Auxiliary database format error */
#define SQLITE_RANGE                       25  /* 2nd parameter to sqlite3_bind out of range */
#define SQLITE_NOTADB                      26  /* File opened that is not a database file */
#define SQLITE_ROW                         100 /* sqlite3_step() has another row ready */
#define SQLITE_DONE                        101 /* sqlite3_step() has finished executing */
/* End-of-Error-Codes */

/* Combination of the following bit values are used
   as the third argument to the sqlite3_open_v2() interface */
#define SQLITE_OPEN_READONLY               1
#define SQLITE_OPEN_READWRITE              2
#define SQLITE_OPEN_CREATE                 4
#define SQLITE_OPEN_DELETEONCLOSE          8
#define SQLITE_OPEN_EXCLUSIVE              16
#define SQLITE_OPEN_MAIN_DB                256
#define SQLITE_OPEN_TEMP_DB                512
#define SQLITE_OPEN_TRANSIENT_DB           1024
#define SQLITE_OPEN_MAIN_JOURNAL           2048
#define SQLITE_OPEN_TEMP_JOURNAL           4096
#define SQLITE_OPEN_SUBJOURNAL             8192
#define SQLITE_OPEN_MASTER_JOURNAL         16384

/* Status Parameters for prepared statements */
#define SQLITE_STMTSTATUS_FULLSCAN_STEP    1
#define SQLITE_STMTSTATUS_SORT             2

/* Authorizer Action Codes */
#define SQLITE_CREATE_INDEX                1   /* Index Name      Table Name      */
#define SQLITE_CREATE_TABLE                2   /* Table Name      NULL            */
#define SQLITE_CREATE_TEMP_INDEX           3   /* Index Name      Table Name      */
#define SQLITE_CREATE_TEMP_TABLE           4   /* Table Name      NULL            */
#define SQLITE_CREATE_TEMP_TRIGGER         5   /* Trigger Name    Table Name      */
#define SQLITE_CREATE_TEMP_VIEW            6   /* View Name       NULL            */
#define SQLITE_CREATE_TRIGGER              7   /* Trigger Name    Table Name      */
#define SQLITE_CREATE_VIEW                 8   /* View Name       NULL            */
#define SQLITE_DELETE                      9   /* Table Name      NULL            */
#define SQLITE_DROP_INDEX                  10  /* Index Name      Table Name      */
#define SQLITE_DROP_TABLE                  11  /* Table Name      NULL            */
#define SQLITE_DROP_TEMP_INDEX             12  /* Index Name      Table Name      */
#define SQLITE_DROP_TEMP_TABLE             13  /* Table Name      NULL            */
#define SQLITE_DROP_TEMP_TRIGGER           14  /* Trigger Name    Table Name      */
#define SQLITE_DROP_TEMP_VIEW              15  /* View Name       NULL            */
#define SQLITE_DROP_TRIGGER                16  /* Trigger Name    Table Name      */
#define SQLITE_DROP_VIEW                   17  /* View Name       NULL            */
#define SQLITE_INSERT                      18  /* Table Name      NULL            */
#define SQLITE_PRAGMA                      19  /* Pragma Name     1st arg or NULL */
#define SQLITE_READ                        20  /* Table Name      Column Name     */
#define SQLITE_SELECT                      21  /* NULL            NULL            */
#define SQLITE_TRANSACTION                 22  /* Operation       NULL            */
#define SQLITE_UPDATE                      23  /* Table Name      Column Name     */
#define SQLITE_ATTACH                      24  /* Filename        NULL            */
#define SQLITE_DETACH                      25  /* Database Name   NULL            */
#define SQLITE_ALTER_TABLE                 26  /* Database Name   Table Name      */
#define SQLITE_REINDEX                     27  /* Index Name      NULL            */
#define SQLITE_ANALYZE                     28  /* Table Name      NULL            */
#define SQLITE_CREATE_VTABLE               29  /* Table Name      Module Name     */
#define SQLITE_DROP_VTABLE                 30  /* Table Name      Module Name     */
#define SQLITE_FUNCTION                    31  /* NULL            Function Name   */
#define SQLITE_SAVEPOINT                   32  /* Operation       Savepoint Name  */
                                           
/* Authorizer Return Codes */              
#define SQLITE_DENY                        1   /* Abort the SQL statement with an error */
#define SQLITE_IGNORE                      2   /* Don't allow access, but don't generate an error */

/* Status Parameters */
#define SQLITE_STATUS_MEMORY_USED          0
#define SQLITE_STATUS_PAGECACHE_USED       1
#define SQLITE_STATUS_PAGECACHE_OVERFLOW   2
#define SQLITE_STATUS_SCRATCH_USED         3
#define SQLITE_STATUS_SCRATCH_OVERFLOW     4
#define SQLITE_STATUS_MALLOC_SIZE          5
#define SQLITE_STATUS_PARSER_STACK         6
#define SQLITE_STATUS_PAGECACHE_SIZE       7
#define SQLITE_STATUS_SCRATCH_SIZE         8

/* Status Parameters for database connections */
#define SQLITE_DBSTATUS_LOOKASIDE_USED     0

/* Run-Time Limit Categories */
#define SQLITE_LIMIT_LENGTH                0
#define SQLITE_LIMIT_SQL_LENGTH            1
#define SQLITE_LIMIT_COLUMN                2
#define SQLITE_LIMIT_EXPR_DEPTH            3
#define SQLITE_LIMIT_COMPOUND_SELECT       4
#define SQLITE_LIMIT_VDBE_OP               5
#define SQLITE_LIMIT_FUNCTION_ARG          6
#define SQLITE_LIMIT_ATTACHED              7
#define SQLITE_LIMIT_LIKE_PATTERN_LENGTH   8
#define SQLITE_LIMIT_VARIABLE_NUMBER       9
#define SQLITE_LIMIT_TRIGGER_DEPTH         10

#endif
