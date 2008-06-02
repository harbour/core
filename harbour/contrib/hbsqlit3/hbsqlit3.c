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
 */

#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbver.h"

#if HB_VER_MAJOR < 1
   #error Too old version [x]Harbour
#endif

#include "sqlite3/sqlite3.c"

#ifdef NODLL
extern char          *sqlite3_temp_directory;
#endif
typedef sqlite3      *psqlite3;
typedef sqlite3_stmt *psqlite3_stmt;

static psqlite3      hb_parsqlite3( int );
static void          hb_retsqlite3( sqlite3 * );

static void          SQL3TraceLog( void *, const char * );
static void          SQL3ProfileLog( void *, const char *, sqlite3_uint64 );

/*
   destructor, it's executed automatically 
*/

static HB_GARBAGE_FUNC( hb_sqlite3_Destructor )
{
   psqlite3 *dbPtr = ( psqlite3 * ) Cargo;

   if( *dbPtr )
   {
      sqlite3  *pDb = ( sqlite3 * ) dbPtr;

      sqlite3_close( pDb );
      *dbPtr = NULL;
   }
}

/*
   hb_par* & hb_ret* for sqlite3*
*/

static psqlite3 hb_parsqlite3( int iParam )
{
   psqlite3 *dbPtr = ( psqlite3 * ) hb_parptrGC( hb_sqlite3_Destructor, iParam );

   if( dbPtr )
   {
      return *dbPtr;
   }
   else
   {
      return NULL;
   }
}

static void hb_retsqlite3( sqlite3 *pDb )
{
   psqlite3 *dbPtr;

   dbPtr = ( psqlite3 * ) hb_gcAlloc( sizeof(psqlite3), hb_sqlite3_Destructor );
   *dbPtr = pDb;
   hb_retptrGC( ( void * ) dbPtr );
}

/*
   sqlite3_libversion()         -> cVersion
   sqlite3_libversion_number()  -> nVersionNumber

   Returns values equivalent to the header constants 
   SQLITE_VERSION and SQLITE_VERSION_NUMBER.
*/

HB_FUNC( SQLITE3_LIBVERSION )
{
   hb_retc( sqlite3_libversion() );
}

HB_FUNC( SQLITE3_LIBVERSION_NUMBER )
{
   hb_retni( sqlite3_libversion_number() );
}

/*
   Enable Or Disable Extended Result Codes

   sqlite3_extended_result_codes( db, lOnOff) -> nResultCode
*/

HB_FUNC( SQLITE3_EXTENDED_RESULT_CODES )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retni( sqlite3_extended_result_codes(db, hb_parl(2)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_EXTENDED_RESULT_CODES", 1, hb_paramError(1) );
   }
}

/*
   Error Codes And Messages

   sqlite3_errcode( db ) -> returns the numeric result code or extended result
                            code 
   sqlite3_errmsg( db )  -> return English-language text 
                            that describes the error
*/

HB_FUNC( SQLITE3_ERRCODE )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retni( sqlite3_errcode(db) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_ERRCODE", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_ERRMSG )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retc( sqlite3_errmsg(db) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_ERRMSG", 1, hb_paramError(1) );
   }
}

/*
   Suspend Execution For A Short Time

   sqlite3_sleep( ms )
*/

HB_FUNC( SQLITE3_SLEEP )
{
   hb_retni( sqlite3_sleep(hb_parni(1)) );
}

/*
   Last Insert Rowid

   sqlite3_last_insert_rowid( db ) -> nROWID
*/

HB_FUNC( SQLITE3_LAST_INSERT_ROWID )
{
   psqlite3       db;
   sqlite3_int64  int64;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      int64 = sqlite3_last_insert_rowid( db );
      #ifndef HB_LONG_LONG_OFF
      hb_retnll( int64 );
      #else
      hb_retnd( int64 );
      #endif
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_LAST_INSERT_ROWID", 1, hb_paramError(1) );
   }
}

/*
   Name Of The Folder Holding Temporary Files

   sqlite3_temp_directory( cDirName ) -> lResult (TRUE/FALSE) 
*/

HB_FUNC( SQLITE3_TEMP_DIRECTORY )
{
   BOOL  bResult = FALSE;

   #ifdef NODLL
   LPSTR pszDirName;

   #ifdef __XHARBOUR__
   pszDirName = hb_fileNameConv( hb_strdup( ( char * ) hb_parc(1) ) );
   #else
   BOOL  fFree;

   pszDirName = hb_fsNameConv( ( BYTE * ) hb_parc(1), &fFree );
   #endif
   if( hb_fsIsDirectory(pszDirName) )
   {
      bResult = TRUE;
   }
   else
   {
      if( hb_parl(2) )  // create temp directory if not exist
      {
         if( hb_fsMkDir(pszDirName) )
         {
            bResult = TRUE;
         }
         else
         {
            HB_TRACE(HB_TR_DEBUG, ("sqlite_temp_directory(): Can't create directory %s", pszDirName));
         }
      }
      else
      {
         HB_TRACE(HB_TR_DEBUG, ("sqlite_temp_directory(): Directory doesn't exist %s", pszDirName));
      }
   }

   if( bResult )
   {
      sqlite3_temp_directory = hb_strdup( pszDirName );
   }

   #ifdef __XHARBOUR__
   hb_xfree( pszDirName );
   #else
   if( fFree )
   {
      hb_xfree( pszDirName );
   }
   #endif
   #endif
   hb_retl( bResult );
}

/*
   Opening( creating ) A New Database Connection

   sqlite3_open( cDatabace, lCreateIfNotExist ) -> return pointer to Db
                                                   or NIL if error occurs
   sqlite3_open_v2( cDatabace, nOpenMode )      -> return pDb or NIL
*/

HB_FUNC( SQLITE3_OPEN )
{
   psqlite3 db;
   LPSTR    pszdbName;

   #ifdef __XHARBOUR__
   pszdbName = hb_fileNameConv( hb_strdup( ( char * ) hb_parc(1) ) );
   #else
   BOOL  fFree;

   pszdbName = ( LPSTR ) hb_fsNameConv( ( BYTE * ) hb_parc(1), &fFree );
   #endif
   if( (!hb_fsFile( ( BYTE * ) pszdbName)) && (!hb_parl(2)) )
   {
      HB_TRACE(HB_TR_DEBUG, ("sqlite3_open(): Database doesn't exist %s", pszdbName));

      hb_retptr( NULL );
      return;
   }

   if( sqlite3_open(pszdbName, &db) == SQLITE_OK )
   {
      hb_retsqlite3( db );
   }
   else
   {
      sqlite3_close( db );

      hb_retptr( NULL );
   }

   #ifdef __XHARBOUR__
   hb_xfree( pszdbName );
   #else
   if( fFree )
   {
      hb_xfree( pszdbName );
   }
   #endif
}

HB_FUNC( SQLITE3_OPEN_V2 )
{
   psqlite3 db;
   LPSTR    pszdbName;

   #ifdef __XHARBOUR__
   pszdbName = hb_fileNameConv( hb_strdup( ( char * ) hb_parc(1) ) );
   #else
   BOOL  fFree;

   pszdbName = ( LPSTR ) hb_fsNameConv( ( BYTE * ) hb_parc(1), &fFree );
   #endif
   if( sqlite3_open_v2(pszdbName, &db, hb_parni(2), NULL) == SQLITE_OK )
   {
      hb_retsqlite3( db );
   }
   else
   {
      sqlite3_close( db );

      hb_retptr( NULL );
   }

   #ifdef __XHARBOUR__
   hb_xfree( pszdbName );
   #else
   if( fFree )
   {
      hb_xfree( pszdbName );
   }
   #endif
}

/*
   One-Step Query Execution Interface
   
   sqlite3_exec( db, cSQLTEXT ) -> nResultCode
*/

HB_FUNC( SQLITE3_EXEC )
{
   psqlite3 db;
   LPSTR    pszErrMsg = 0;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      int   rc = sqlite3_exec( db, hb_parc(2), NULL, 0, &pszErrMsg );

      if( rc != SQLITE_OK )
      {
         HB_TRACE(HB_TR_DEBUG, ("sqlite3_exec(): Returned error: %s", pszErrMsg));
         sqlite3_free( pszErrMsg );
      }

      hb_retni( rc );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_EXEC", 1, hb_paramError(1) );
   }
}

/*
   Compiling An SQL Statement

   sqlite3_prepare( db, cSQLTEXT ) 
   -> return pointer to compiled statement or NIL if error occurs

   TODO: pszTail?
*/

HB_FUNC( SQLITE3_PREPARE )
{
   psqlite3 pDb;

   pDb = ( psqlite3 ) hb_parsqlite3( 1 );
   if( pDb != NULL )
   {
      PHB_ITEM SQL = hb_param( 2, HB_IT_STRING );

      if( SQL != NULL )
      {
         LPSTR          pSQL = hb_itemGetCPtr( SQL );
         ULONG          ulLen = hb_itemGetCLen( SQL );
         psqlite3_stmt  pStmt;
         const char     *pszTail;
         int            result;

         result = sqlite3_prepare_v2( pDb, pSQL, ulLen, &pStmt, &pszTail );
         if( result == SQLITE_OK )
         {
            hb_retptr( pStmt );
         }
         else
         {
            sqlite3_finalize( pStmt );

            hb_retptr( NULL );
         }
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_PREPARE", 1, hb_paramError(2) );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_PREPARE", 1, hb_paramError(1) );
   }
}

/*
   Find The Database Handle Associated With A Prepared Statement

   sqlite3_db_handle( pStmt ) -> pDb
*/

HB_FUNC( SQLITE3_DB_HANDLE )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retsqlite3( sqlite3_db_handle(pStmt) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_DB_HANDLE", 1, hb_paramError(1) );
   }
}

/*
   Evaluate An Prepared SQL Statement
 
   sqlite3_step( pStmt ) -> nResultCode
*/

HB_FUNC( SQLITE3_STEP )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_step(pStmt) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_STEP", 1, hb_paramError(1) );
   }
}

/*
   Reset All Bindings On A Prepared Statement

   sqlite3_clear_bindings( pStmt ) -> nResultCode

   Contrary to the intuition of many, 
   sqlite3_reset() does not reset the bindings on a prepared statement. 
   Use this routine to reset all host parameters to NULL.
*/

HB_FUNC( SQLITE3_CLEAR_BINDINGS )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_clear_bindings(pStmt) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_CLEAR_BINDINGS", 1, hb_paramError(1) );
   }
}

/*
   Reset A Prepared Statement Object

   sqlite3_reset( pStmt ) -> nResultCode
*/

HB_FUNC( SQLITE3_RESET )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_reset(pStmt) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_RESET", 1, hb_paramError(1) );
   }
}

/*
   Finalize A Prepared Statement Object

   sqlite3_finalize( pStmt ) -> nResultCode
*/

HB_FUNC( SQLITE3_FINALIZE )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_finalize(pStmt) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_FINALIZE", 1, hb_paramError(1) );
   }
}

/*
int sqlite3_bind_blob(sqlite3_stmt*, int, const void*, int n, void(*)(void*));
int sqlite3_bind_double(sqlite3_stmt*, int, double);
int sqlite3_bind_int(sqlite3_stmt*, int, int);
int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64);
int sqlite3_bind_null(sqlite3_stmt*, int);
int sqlite3_bind_text(sqlite3_stmt*, int, const char*, int n, void(*)(void*));
int sqlite3_bind_value(sqlite3_stmt*, int, const sqlite3_value*);
int sqlite3_bind_zeroblob(sqlite3_stmt*, int, int n)
*/

/*
   Binding Values To Prepared Statements

   These routines return SQLITE_OK on success or an error code if anything 
   goes wrong. 
   SQLITE_RANGE is returned if the parameter index is out of range. 
   SQLITE_NOMEM is returned if malloc fails. 
   SQLITE_MISUSE is returned if these routines are called on a virtual machine
   that is the wrong state or which has already been finalized.
*/

HB_FUNC( SQLITE3_BIND_BLOB )
{
   psqlite3_stmt  pStmt;
   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_blob(pStmt, hb_parni(2), ( const char * ) hb_parcx(3), hb_parcsiz(3)-1, SQLITE_TRANSIENT) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_BLOB", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_BIND_DOUBLE )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_double(pStmt, hb_parni(2), hb_parnd(3)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_DOUBLE", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_BIND_INT )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_int(pStmt, hb_parni(2), hb_parni(3)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_INT", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_BIND_INT64 )
{
   psqlite3_stmt  pStmt;
   sqlite3_int64  int64;

   #ifndef HB_LONG_LONG_OFF
   int64 = hb_parnll( 3 );
   #else
   int64 = hb_parnd( 3 );
   #endif
   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_int64(pStmt, hb_parni(2), int64) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_INT64", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_BIND_NULL )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_null(pStmt, hb_parni(2)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_NULL", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_BIND_TEXT )
{
   psqlite3_stmt  pStmt;
   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_text(pStmt, hb_parni(2), ( const char * ) hb_parc(3), hb_parclen(3), SQLITE_TRANSIENT) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_TEXT", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_BIND_ZEROBLOB )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_zeroblob(pStmt, hb_parni(2), hb_parni(3)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_ZEROBLOB", 1, hb_paramError(1) );
   }
}

/*
   Number Of Host Parameters

   sqlite3_bind_parameter_count( pStmt ) -> nResult
*/

HB_FUNC( SQLITE3_BIND_PARAMETER_COUNT )
{
   psqlite3_stmt  pStmt;
   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_parameter_count(pStmt) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_PARAMETER_COUNT", 1, hb_paramError(1) );
   }
}

/*
   Index Of A Parameter With A Given Name

   sqlite3_bind_parameter_index( pStmt, cParameterName ) -> nResult
*/

HB_FUNC( SQLITE3_BIND_PARAMETER_INDEX )
{
   psqlite3_stmt  pStmt;
   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_bind_parameter_index(pStmt, hb_parc(2)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_PARAMETER_INDEX", 1, hb_paramError(1) );
   }
}

/*
   Name Of A Host Parameter

   sqlite3_bind_parameter_name( pStmt, nParameterIndex ) -> cParameterName
*/

HB_FUNC( SQLITE3_BIND_PARAMETER_NAME )
{
   psqlite3_stmt  pStmt;
   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retc( sqlite3_bind_parameter_name(pStmt, hb_parni(2)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BIND_PARAMETER_NAME", 1, hb_paramError(1) );
   }
}

/*
   Count The Number Of Rows Modified

   sqlite3_changes( db ) -> nRowCount
*/

HB_FUNC( SQLITE3_CHANGES )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retni( sqlite3_changes(db) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_CHANGES", 1, hb_paramError(1) );
   }
}

/*
   Total Number Of Rows Modified

   sqlite3_total_changes( db ) -> nRowCount
*/

HB_FUNC( SQLITE3_TOTAL_CHANGES )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retni( sqlite3_total_changes(db) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_TOTAL_CHANGES", 1, hb_paramError(1) );
   }
}

/*
   Number Of Columns In A Result Set

   sqlite3_column_count( pStmt ) -> nColumnCount
*/

HB_FUNC( SQLITE3_COLUMN_COUNT )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_column_count(pStmt) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_COUNT", 1, hb_paramError(1) );
   }
}

/*
   sqlite3_column_type( pStmt, nIndex ) -> nColumnType

   nColumnType is Datatype code for the initial data type of the result column

   SQLITE_INTEGER      1
   SQLITE_FLOAT        2
   SQLITE_TEXT         3
   SQLITE3_TEXT        3
   SQLITE_BLOB         4
   SQLITE_NULL         5
   
   Declared Datatype Of A Query Result (see doc)
   sqlite3_column_decltype( pStmt, nIndex ) -> nColumnDeclType
*/

HB_FUNC( SQLITE3_COLUMN_TYPE )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_column_type(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_TYPE", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_DECLTYPE )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retc( sqlite3_column_decltype(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_DECLTYPE", 1, hb_paramError(1) );
   }
}

/*
   Column Names In A Result Set

   sqlite3_column_name( pStmt, columnIndex) -> columnName
*/

HB_FUNC( SQLITE3_COLUMN_NAME )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retc( sqlite3_column_name(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_NAME", 1, hb_paramError(1) );
   }
}

/*
   sqlite3_column_bytes( pStmt, columnIndex ) 
   -> returns the number of bytes in that BLOB or string

   Results Values From A Query

   sqlite3_column_blob( pStmt, columnIndex )   -> value as BLOB
   sqlite3_column_double( pStmt, columnIndex ) -> value as double
   sqlite3_column_int( pStmt, columnIndex )    -> value as integer
   sqlite3_column_int64( pStmt, columnIndex )  -> value as long long
   sqlite3_column_text( pStmt, columnIndex )   -> value as text
*/

HB_FUNC( SQLITE3_COLUMN_BYTES )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_column_bytes(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_BYTES", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_BLOB )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      int   index = hb_parni( 2 ) - 1;
      hb_retclen( sqlite3_column_blob(pStmt, index), sqlite3_column_bytes(pStmt, index) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_BLOB", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_DOUBLE )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retnd( sqlite3_column_double(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_DOUBLE", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_INT )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retni( sqlite3_column_int(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_INT", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_INT64 )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      #ifndef HB_LONG_LONG_OFF
      hb_retnll( sqlite3_column_int64(pStmt, hb_parni(2) - 1) );
      #else
      hb_retnd( sqlite3_column_int64(pStmt, hb_parni(2) - 1) );
      #endif
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_INT64", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_TEXT )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      int   index = hb_parni( 2 ) - 1;
      hb_retclen( ( char * ) sqlite3_column_text(pStmt, index), sqlite3_column_bytes(pStmt, index) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_TEXT", 1, hb_paramError(1) );
   }
}

/*
   Enable Or Disable Extension Loading

   sqlite3_enable_load_extension( db, lOnOff ) -> prev.state
*/

HB_FUNC( SQLITE3_ENABLE_LOAD_EXTENSION )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retni( sqlite3_enable_load_extension(db, hb_parl(2)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_ENABLE_LOAD_EXTENSION", 1, hb_paramError(1) );
   }
}

/*
   Reset Automatic Extension Loading

   sqlite3_reset_auto_extension()
*/

HB_FUNC( SQLITE3_RESET_AUTO_EXTENSION )
{
   sqlite3_reset_auto_extension();
}

/*
   Set A Busy Timeout

   sqlite3_busy_timeout( db, ms )
*/

HB_FUNC( SQLITE3_BUSY_TIMEOUT )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retni( sqlite3_busy_timeout(db, hb_parni(2)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_ENABLE_LOAD_EXTENSION", 1, hb_paramError(1) );
   }
}

/*
   Determine If An SQL Statement Is Complete

   sqlite3_complete( sqlText ) -> lResult
*/

HB_FUNC( SQLITE3_COMPLETE )
{
   hb_retl( sqlite3_complete(hb_parc(1)) );
}

/*
   Convenience Routines For Running Queries

   sqlite3_get_table( db, sqlText ) -> aResult
*/

HB_FUNC( SQLITE3_GET_TABLE )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      PHB_ITEM pResultList = hb_itemArrayNew( 0 );
      int      iRow, iCol;
      LPSTR    pszErrMsg = 0;
      char     **pResult;

      if( sqlite3_get_table(db, hb_parc(2), &pResult, &iRow, &iCol, &pszErrMsg) == SQLITE_OK )
      {
         int   i, j, k = 0;
         #ifdef __XHARBOUR__
         PHB_ITEM pTemp = hb_itemNew( NULL );
         #endif

         for( i = 0; i < iRow + 1; i++ )
         {
            PHB_ITEM pArray = hb_itemArrayNew( iCol );

            for( j = 1; j <= iCol; j++, k++ )
            {
               if( pResult[k] != NULL )
               {
                  #ifdef __XHARBOUR__
                  hb_arraySetForward( pArray, j, hb_itemPutC(pTemp, pResult[k]) );
                  #else
                  hb_arraySetC( pArray, j, pResult[k] );
                  #endif
               }
               else
               {
                  #ifdef __XHARBOUR__
                  hb_arraySetForward( pArray, j, hb_itemPutC(pTemp, "NULL") );
                  #else
                  hb_arraySetC( pArray, j, "NULL" );
                  #endif
               }
            }

            hb_arrayAddForward( pResultList, pArray );
            hb_itemRelease( pArray );
         }

         #ifdef __XHARBOUR__
         hb_itemRelease( pTemp );
         #endif
      }
      else
      {
         HB_TRACE(HB_TR_DEBUG, ("sqlite3_get_table(): Returned error: %s", pszErrMsg));
         sqlite3_free( pszErrMsg );
      }

      sqlite3_free_table( pResult );

      #ifdef __XHARBOUR__
      hb_itemRelease( hb_itemReturn(pResultList) );
      #else
      hb_itemReturnRelease( pResultList );
      #endif
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_GET_TABLE", 1, hb_paramError(1) );
   }
}

#ifdef SQLITE_ENABLE_COLUMN_METADATA

/*
   Extract Metadata About A Column Of A Table
   based on
   int sqlite3_table_column_metadata( 
     sqlite3 *db,                - IN:  Connection handle 
     const char *zDbName,        - IN:  Database name or NULL 
     const char *zTableName,     - IN:  Table name 
     const char *zColumnName,    - IN:  Column name 
     char const **pzDataType,    - OUT: Declared data type 
     char const **pzCollSeq,     - OUT: Collation sequence name 
     int *pNotNull,              - OUT: True if NOT NULL constraint exists 
     int *pPrimaryKey,           - OUT: True if column part of PK 
     int *pAutoinc               - OUT: True if column is auto-increment 
);
*/

HB_FUNC( SQLITE3_TABLE_COLUMN_METADATA )
{
   psqlite3 pDb;

   pDb = ( psqlite3 ) hb_parsqlite3( 1 );
   if( pDb != NULL )
   {
      char const  **pzDataType;
      char const  **pzCollSeq;
      int         iNotNull;
      int         iPrimaryKey;
      int         iAutoinc;

      if
      (
         SQLITE_OK == sqlite3_table_column_metadata
            (
               pDb,
               ( const char * ) hb_parc(2),
               ( const char * ) hb_parc(3),
               ( const char * ) hb_parc(4),
               ( char const ** ) &pzDataType,
               ( char const ** ) &pzCollSeq,
               &iNotNull,
               &iPrimaryKey,
               &iAutoinc
            )
      )
      {
         PHB_ITEM pArray = hb_itemArrayNew( 5 );
         #ifdef __XHARBOUR__
         PHB_ITEM pTemp = hb_itemNew( NULL );

         hb_arraySetForward( pArray, 1, hb_itemPutC(pTemp, ( char * ) pzDataType) );
         hb_arraySetForward( pArray, 2, hb_itemPutC(pTemp, ( char * ) pzCollSeq) );

         hb_arraySetForward( pArray, 3, hb_itemPutL(pTemp, iNotNull) );
         hb_arraySetForward( pArray, 4, hb_itemPutL(pTemp, iPrimaryKey) );
         hb_arraySetForward( pArray, 5, hb_itemPutL(pTemp, iAutoinc) );
         hb_itemRelease( pTemp );
         #else
         hb_arraySetC( pArray, 1, ( char * ) pzDataType );
         hb_arraySetC( pArray, 2, ( char * ) pzCollSeq );

         hb_arraySetL( pArray, 3, (BOOL) iNotNull );
         hb_arraySetL( pArray, 4, (BOOL) iPrimaryKey );
         hb_arraySetL( pArray, 5, (BOOL) iAutoinc );
         #endif

         #ifdef __XHARBOUR__
         hb_itemRelease( hb_itemReturn(pArray) );
         #else
         hb_itemReturnRelease( pArray );
         #endif
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_TABLE_COLUMN_METADATA", 1, hb_paramError(1) );
   }
}

/*
   Source Of Data In A Query Result

   sqlite3_column_database_name( pStmt, ColumnIndex) -> cDatabaseName
   sqlite3_column_table_name( pStmt, ColumnIndex)    -> cTableName
   sqlite3_column_origin_name( pStmt, ColumnIndex)   -> cColumnName
*/

HB_FUNC( SQLITE3_COLUMN_DATABASE_NAME )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retc( sqlite3_column_database_name(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_DATABASE_NAME", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_TABLE_NAME )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retc( sqlite3_column_table_name(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_TABLE_NAME", 1, hb_paramError(1) );
   }
}

HB_FUNC( SQLITE3_COLUMN_ORIGIN_NAME )
{
   psqlite3_stmt  pStmt;

   pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

   if( pStmt != NULL )
   {
      hb_retc( sqlite3_column_origin_name(pStmt, hb_parni(2) - 1) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_COLUMN_ORIGIN_NAME", 1, hb_paramError(1) );
   }
}
#endif

/*
   BLOB I/O 
*/

/*
   int sqlite3_blob_open(
     sqlite3*,
     const char *zDb,
     const char *zTable,
     const char *zColumn,
     sqlite3_int64 iRow,
     int flags,
     sqlite3_blob **ppBlob
);

   Open A BLOB For Incremental I/O

   Open a handle to the blob located in row iRow, column zColumn, table zTable 
   in database zDb. i.e. the same blob that would be selected by:
 
   SELECT zColumn FROM zDb.zTable WHERE rowid = iRow;
*/

HB_FUNC( SQLITE3_BLOB_OPEN )
{
   psqlite3 pDb;

   pDb = ( psqlite3 ) hb_parsqlite3( 1 );
   if( pDb != NULL )
   {
      sqlite3_blob   **ppBlob;
      sqlite3_int64  iRow;

      #ifndef HB_LONG_LONG_OFF
      iRow = hb_parnll( 5 );
      #else
      iRow = hb_parnd( 5 );
      #endif
      if
      (
         SQLITE_OK == sqlite3_blob_open
            (
               pDb,
               ( const char * ) hb_parc(2),
               ( const char * ) hb_parc(3),
               ( const char * ) hb_parc(4),
               iRow,
               hb_parni(6),
               (HANDLE) & ppBlob
            )
      )
      {
         hb_retptr( ppBlob );
      }
      else
      {
         hb_retptr( NULL );
      }
   }
   else
   {
      hb_retptr( NULL );
   }
}

/*
   Close A BLOB Handle
*/

HB_FUNC( SQLITE3_BLOB_CLOSE )
{
   sqlite3_blob   *pBlob;

   pBlob = ( sqlite3_blob * ) hb_parptr( 1 );
   if( pBlob != NULL )
   {
      hb_retni( sqlite3_blob_close(pBlob) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BLOB_CLOSE", 1, hb_paramError(1) );
   }
}

/*
   Return The Size Of An Open BLOB
*/

HB_FUNC( SQLITE3_BLOB_BYTES )
{
   sqlite3_blob   *pBlob;

   pBlob = ( sqlite3_blob * ) hb_parptr( 1 );
   if( pBlob != NULL )
   {
      hb_retni( sqlite3_blob_bytes(pBlob) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BLOB_BYTES", 1, hb_paramError(1) );
   }
}

/*
   Read Data From A BLOB Incrementally
*/

HB_FUNC( SQLITE3_BLOB_READ )
{
   sqlite3_blob   *pBlob;

   pBlob = ( sqlite3_blob * ) hb_parptr( 1 );
   if( pBlob != NULL )
   {
      int   iLen = hb_parni( 2 );
      BYTE  *buffer;

      if( 0 == iLen )
      {
         iLen = sqlite3_blob_bytes( pBlob );
      }

      buffer = ( BYTE * ) hb_xgrab( iLen + 1 );
      /*hb_xmemset( buffer, 0, iLen );*/

      if( SQLITE_OK == sqlite3_blob_read(pBlob, buffer, iLen, hb_parni(3))) 
      {
         buffer[iLen] = '\0';
         hb_retclen_buffer( ( char * ) buffer, iLen );
      }
      else
      {
         hb_xfree( buffer );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BLOB_READ", 1, hb_paramError(1) );
   }
}

/*
   Write Data Into A BLOB Incrementally
*/

HB_FUNC( SQLITE3_BLOB_WRITE )
{
   sqlite3_blob   *pBlob;

   pBlob = ( sqlite3_blob * ) hb_parptr( 1 );
   if( pBlob != NULL )
   {
      int iLen = hb_parni( 3 );

      if( iLen == 0 )
         iLen = hb_parcsiz( 2 ) - 1;

      hb_retni( sqlite3_blob_write(pBlob, hb_parcx(2), iLen, hb_parni(4)) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_BLOB_WRITE", 1, hb_paramError(1) );
   }
}

/*
    Test To See If The Database Is In Auto-Commit Mode

    sqlite3_get_autocommit( db ) -> lResult (TRUE/FALSE)
*/

HB_FUNC( SQLITE3_GET_AUTOCOMMIT )
{
   psqlite3 db;

   db = ( psqlite3 ) hb_parsqlite3( 1 );
   if( db != NULL )
   {
      hb_retl( sqlite3_get_autocommit(db) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_GET_AUTOCOMMIT", 1, hb_paramError(1) );
   }
}

/*
   Enable Or Disable Shared Pager Cache

   sqlite3_enable_shared_cache( lOnOff ) -> nResultCode
*/

HB_FUNC( SQLITE3_ENABLE_SHARED_CACHE )
{
   hb_retni( sqlite3_enable_shared_cache(hb_parl(1)) );
}


/*
    Tracing And Profiling Functions

    sqlite3_trace( db, lOnOff )
    sqlite3_profile( db, lOnOff ) 
*/

static void SQL3ProfileLog( void *sFile, const char *sProfileMsg, sqlite3_uint64 int64 )
{
   if( sProfileMsg )
   {
      FILE * hFile = fopen( sFile ? ( const char * ) sFile : "hbsq3_pr.log", "a" );
      
      if( hFile )
      {
         fprintf( hFile, "%s - %lld\n", sProfileMsg, &int64 );
         fclose( hFile );
      }
   }
}

static void SQL3TraceLog( void *sFile, const char *sTraceMsg )
{
   if( sTraceMsg )
   {
      FILE * hFile = fopen( sFile ? ( const char * ) sFile : "hbsq3_tr.log", "a" );
      
      if( hFile )
      {
         fprintf( hFile, "%s \n", sTraceMsg );
         fclose( hFile );
      }
   }
}

HB_FUNC( SQLITE3_PROFILE )
{
   psqlite3 db = ( psqlite3 ) hb_parsqlite3( 1 );

   if( db )
      sqlite3_profile( db, hb_parl( 2 ) ? SQL3ProfileLog : NULL, NULL );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_PROFILELOG", 1, hb_paramError(1) );
}

HB_FUNC( SQLITE3_TRACE )
{
   psqlite3 db = ( psqlite3 ) hb_parsqlite3( 1 );

   if( db )
      sqlite3_trace( db, hb_parl( 2 ) ? SQL3TraceLog : NULL, NULL );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "SQLITE3_TRACELOG", 1, hb_paramError(1) );
}

/*
   BLOB Import/export 
*/

HB_FUNC( SQLITE3_FILE_TO_BUFF )
{
   BYTE  *buffer = NULL;
   int   handle = hb_fsOpen( ( BYTE * ) hb_parc(1), FO_READ );
   ULONG iSize;

   if( handle != FS_ERROR )
   {
      iSize = ( int ) hb_fsSeek( handle, 0, FS_END );
      iSize -= ( int ) hb_fsSeek( handle, 0, FS_SET );
      buffer = ( BYTE * ) hb_xgrab( iSize + 1 );
      iSize = hb_fsReadLarge( handle, buffer, iSize );
      buffer[iSize] = '\0';
      hb_fsClose( handle );
   }
   else
      iSize = 0;

   hb_retclen_buffer( ( char * ) buffer, iSize );
}

HB_FUNC( SQLITE3_BUFF_TO_FILE )
{
   int   handle = hb_fsCreate( ( BYTE * ) hb_parc(1), FC_NORMAL );
   ULONG iSize = hb_parcsiz( 2 ) - 1;

   if( (handle != FS_ERROR) && (iSize > 0) )
   {
      if( iSize == hb_fsWriteLarge(handle, ( BYTE * ) hb_parcx(2), iSize) )
         hb_retni( 0 );
      else
         hb_retni( -1 );

      hb_fsClose( handle );
   }
   else
      hb_retni( 1 );
}

/*
   TODO?: Formatted String Printing Functions

   char *sqlite3_mprintf(const char*,...);
   char *sqlite3_vmprintf(const char*, va_list);
   char *sqlite3_snprintf(int,char*,const char*, ...);
*/
