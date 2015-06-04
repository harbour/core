/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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


#include <windows.h>
#include <shlobj.h>
#include <commctrl.h>
#include <olectl.h>
#include <stdio.h>
#include <ocidl.h>

#include "hbapiitm.h"
#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapierr.h"
#include "c_api.h"

typedef int __hbint;

#define false                                     FALSE
#define true                                      TRUE
#define bool                                      BOOL
#define CACHE_MAX_BYTES                           32000
#define MAX_CONNECTIONS                           20
#define MAX_QUERIES                               255


static h_connection                               conn;
static h_database                                 db;
static h_connection                               _conn[ MAX_CONNECTIONS ];
static h_database                                 _db[ MAX_CONNECTIONS ];
static h_query                                    _qry[ MAX_QUERIES ];
static h_class_def                                _cls;
static h_mtd_def                                  _mtd;
static h_prop_def                                 _prp;


#ifdef __HB_MT__
   #include "hbthread.h"
   static PHB_ITEM s_pMtx = NULL;
#endif


void raise_exception( int err )
{
   /* Socket Errors */
   if( err > 10000 )
   {
      hb_errRT_BASE_SubstR( EG_NOALIAS, err /*17071*/, "Cache Server Connection Lost", "CACHERDD", 1, NULL );
   }
}


bool cbind_call( int rc )
{
    if( rc != 0 )
    {
       return FALSE;
    }
    else
    {
       return TRUE;
    }
}


bool cache_connect( h_connection* pconn, h_database* pdb, char* conn_str, char* username, char* password, int timeout )
{
    WORD wconn_str[ MAX_PATH ];
    WORD wusername[ MAX_PATH ];
    WORD wpassword[ MAX_PATH ];

    MultiByteToWideChar( CP_ACP, 0, conn_str, -1, ( wchar_t* ) wconn_str, MAX_PATH );
    MultiByteToWideChar( CP_ACP, 0, username, -1, ( wchar_t* ) wusername, MAX_PATH );
    MultiByteToWideChar( CP_ACP, 0, password, -1, ( wchar_t* ) wpassword, MAX_PATH );

    cbind_set_ignore_null( 0 );

    if( ! cbind_call( cbind_alloc_conn( ( const wchar_t* ) wconn_str,
                                        ( const wchar_t* ) wusername,
                                        ( const wchar_t* ) wpassword,
                                        timeout, pconn ) ) )
    {
       conn = ( h_connection ) NULL;
       db   = ( h_database ) NULL;
       cbind_set_ignore_null( 1 );
       return false;
    }
    if( ! cbind_call( cbind_alloc_db( *pconn, pdb ) ) )
    {
       cbind_call( cbind_free_conn( *pconn ) );
       conn = ( h_connection ) NULL;
       db   = ( h_database ) NULL;
       cbind_set_ignore_null( 1 );
       return false;
    }
#ifdef __HB_MT__
    s_pMtx = hb_threadMutexCreate();
#endif

    return true;
}


HB_FUNC( CACHEGETLASTERRORCODE )
{
   /* hb_retni( cbind_get_last_err_code() ); */
}


HB_FUNC( CACHECONNECT )
{
   h_connection conn_;
   h_database   db_;
   bool_t       bOk;
   int          i;
   int          iConxn = 0;

   bOk = cache_connect( &conn_, &db_, ( char * ) hb_parcx( 1 ), ( char * ) hb_parc( 2 ), ( char * ) hb_parc( 3 ), hb_parnint( 4 ) );

   if( bOk )
   {
      bOk = FALSE;
      for( i = 1; i < MAX_CONNECTIONS; i++ )
      {
         if( _conn[ i ] == ( h_connection ) NULL )
         {
            bOk = TRUE;
            break;
         }
      }
      if( bOk )
      {
         _conn[ i ] = conn_;
         _db[ i ] = db_;
         iConxn = i;
      }
   }
   hb_retnint( iConxn );
}


HB_FUNC( CACHEDISCONNECT )
{
   int i;

   for ( i = 1; i < MAX_CONNECTIONS; i++ )
   {
      if( _conn[ i ] != ( h_connection ) NULL )
      {
         cbind_free_db( _db[ i ] );
         cbind_free_conn( _conn[ i ] );
         _conn[ i ] = ( h_connection ) NULL;
         _db[ i ] = ( h_database ) NULL;
      }
   }
   cbind_set_ignore_null( 1 );
#ifdef __HB_MT__
   if( s_pMtx )
   {
      hb_threadMutexUnlock( s_pMtx );
   }
#endif
}


HB_FUNC( CACHEDISCONNECTTHIS )
{
   int    i        = hb_parnint( 1 );
   bool_t bSuccess = 0;

   if( i <= MAX_CONNECTIONS )
   {
      if( _conn[ i ] != ( h_connection ) NULL )
      {
         cbind_free_db( _db[ i ] );
         cbind_free_conn( _conn[ i ] );
         _conn[ i ] = ( h_connection ) NULL;
         _db[ i ] = ( h_database ) NULL;

         bSuccess = 1;
      }
   }
   cbind_set_ignore_null( 1 );
#ifdef __HB_MT__
   if( s_pMtx )
   {
      hb_threadMutexUnlock( s_pMtx );
   }
#endif
   hb_retl( bSuccess );
}


/* CachePrepare( nConxn, nWA, cTable, cOpenInfo ) => nWA | 0
*/
HB_FUNC( CACHEPREPARE )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 3 ), strlen( hb_parc( 3 ) ), MULTIBYTE, 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 4 ), strlen( hb_parc( 4 ) ), MULTIBYTE, 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddPrepare" ) );

      cbind_get_arg_as_int( db, 3, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheGetField( nConxn, nWA, nRecordNum, cFieldID ) => cFieldValue
*/
HB_FUNC( CACHEGETFIELD )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   char        arg1_ref_val[ 1024 ];
   const int   arg1_ref_cap = 1024;
   byte_size_t arg1_ref_size;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 4 ), strlen( hb_parc( 4 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_STRING_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddGetField" ) );

      cbind_get_arg_as_str( db, 3, arg1_ref_val, arg1_ref_cap, MULTIBYTE, &arg1_ref_size, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retclen( arg1_ref_val, arg1_ref_size );
}


/* CacheGetRecord( nConxn, nWA, nRecNo ) => cRecordInfoAsString
*/
HB_FUNC( CACHEGETRECORD )
{
#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      h_database  db = _db[ hb_parni( 1 ) ];
      char        arg1_ref_val[ CACHE_MAX_BYTES ];
      const int   arg1_ref_cap = CACHE_MAX_BYTES;
      byte_size_t arg1_ref_size;
      bool_t      is_null;

      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_STRING_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddGetRecord" ) );
      cbind_get_arg_as_str( db, 2, arg1_ref_val, arg1_ref_cap, MULTIBYTE, &arg1_ref_size, &is_null );

      if( is_null )
      {
         hb_retc( "" );
      }
      else
      {
         hb_retclen( arg1_ref_val, arg1_ref_size );
      }
#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
}


/* CacheInsert( nConxn, nWA ) => nRecordID
*/
HB_FUNC( CACHEINSERT )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddInsert" ) );
      cbind_get_arg_as_double( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheSetFieldBinary( nConxn, nWA, nRecNo, cFieldsInfo, binaryData, nLength, nMode ) => nRecordNum
*/
HB_FUNC( CACHESETFIELDBINARY )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 4 ), strlen( hb_parc( 4 ) ), MULTIBYTE, 1 );

      /* This is to be implemented judiciously */
      /* cbind_set_next_arg_as_str( db, hb_parc( 5 ), hb_parnint( 6 ), MULTIBYTE, 1 ); */
      cbind_set_next_arg_as_bin( db, hb_parc( 5 ), hb_parnint( 6 ), 1 );

      cbind_set_next_arg_as_int( db, hb_parnint( 6 ), 0 );
      cbind_set_next_arg_as_int( db, hb_parnint( 7 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddSetFieldBinary" ) );
      cbind_get_arg_as_double( db, 6, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheGetFieldBinary( nConxn, nWA, nRecNo, cFieldsInfo, nMode ) => cBinaryValue
*/
HB_FUNC( CACHEGETFIELDBINARY )
{
   h_database  db   = _db[ hb_parni( 1 ) ];
   char        arg1_ref_val[ CACHE_MAX_BYTES ];
   const int   arg1_ref_cap = CACHE_MAX_BYTES;
   byte_size_t arg1_ref_size;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 4 ), strlen( hb_parc( 4 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_int( db, hb_parnint( 5 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_BINARY_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddGetFieldBinary" ) );
      cbind_get_arg_as_bin( db, 4, arg1_ref_val, arg1_ref_cap, &arg1_ref_size, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retclen( arg1_ref_val, arg1_ref_size );
}


/* CacheSetFields( nConxn, nWA, nRecNo, cFieldsInfo ) => nRecordNum
*/
HB_FUNC( CACHESETFIELDS )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 4 ), strlen( hb_parc( 4 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddSetFields" ) );
      cbind_get_arg_as_double( db, 3, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheLockMode( nConxn, nMode ) => nPreviousMode
*/
HB_FUNC( CACHELOCKMODE )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddInsertLockMode" ) );
      cbind_get_arg_as_int( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheLockSeconds( nConxn, nMode ) => nPreviousTimeout
*/
HB_FUNC( CACHELOCKSECONDS )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddLockTimeOut" ) );
      cbind_get_arg_as_int( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheSetIndexPos( nConxn, nWA, nRecordNum, cIndexKey ) => nRecordNum
*/
HB_FUNC( CACHESETINDEXPOS )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );
      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 4 ), strlen( hb_parc( 4 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddSetIndexPos" ) );
      cbind_get_arg_as_double( db, 3, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheGoPhantom( nConxn, nWA )   =>  1
*/
HB_FUNC( CACHEGOPHANTOM )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddGoPhantom" ) );
      cbind_get_arg_as_int( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheGoTop( nConxn, nWA ) => nRecordNum
*/
HB_FUNC( CACHEGOTOP )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddGoTop" ) );
      cbind_get_arg_as_double( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheGoBottom( nConxn, nWA ) => nRecordNum
*/
HB_FUNC( CACHEGOBOTTOM )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddGoBottom" ) );
      cbind_get_arg_as_double( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheSeek( nConxn, nWA, cSeek ) => nRecordNum
*/
HB_FUNC( CACHESEEK )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 3 ), strlen( hb_parc( 3 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddSeek" ) );
      cbind_get_arg_as_double( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheSkip( nConxn, nWA, nCurRecord, nRecsToSkip, lRePos ) => nRecordNum
*/
HB_FUNC( CACHESKIP )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_int( db, hb_parnint( 4 ), 0 );
      cbind_set_next_arg_as_bool( db, hb_parl( 5 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddSkip" ) );
      cbind_get_arg_as_double( db, 4, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheCreateTable( nConxn, cTable, cStructInfo ) => lSuccess
*/
HB_FUNC( CACHECREATETABLE )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_str( db, hb_parc( 2 ), strlen( hb_parc( 2 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_str( db, hb_parc( 3 ), strlen( hb_parc( 3 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddCreate" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/* CacheCreateTableExt( nConxn, cTable, cStructInfo ) => lSuccess
*/
HB_FUNC( CACHECREATETABLEEXT )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_str( db, hb_parc( 2 ), strlen( hb_parc( 2 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_str( db, hb_parc( 3 ), strlen( hb_parc( 3 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddCreateTablesExt" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/*  CacheOrdListFocus( nConxn, nWA, nRecordNum, nIndexOrder ) => nOldOrder
*/
HB_FUNC( CACHEORDLISTFOCUS )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_int( db, hb_parnint( 4 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddOrdListFocus" ) );
      cbind_get_arg_as_int( db, 3, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheOrdListAdd( nConxn, nWA, cOrdbag ) => nNumOfIndexes
*/
HB_FUNC( CACHEORDLISTADD )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 3 ), strlen( hb_parc( 3 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddOrdListAdd" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheOrdListRebuild( nConxn, nWA, cTagName )
*/
HB_FUNC( CACHEORDLISTREBUILD )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 3 ), strlen( hb_parc( 3 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddOrdListRebuild" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


HB_FUNC( CACHERELEASE )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      /* NEVER raise exception as QUIT closes all connections before RDD calls CACHE_CLOSE() */
      cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddRelease" );
      //raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddRelease" ) );

      cbind_get_arg_as_int( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/* CacheRecLock( nConxn, nWA, nRecNo ) => 1, 0
*/
HB_FUNC( CACHERECLOCK )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db,  hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddLock" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheFileLock( nConxn, nWA ) => nStatus 1, 0
*/
HB_FUNC( CACHEFILELOCK )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddFileLock" ) );
      cbind_get_arg_as_int( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/* CacheUnlock( nConxn, nWA, nRecNo ) => nStatus
*/
HB_FUNC( CACHEUNLOCK )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db,  hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddUnlock" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheIsLocked( nConxn, nWA, nRecNo ) => nStatus
*/
HB_FUNC( CACHEISLOCKED )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db,  hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"IsRecLocked" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/* CaceRddLockList( nConxn, nWA )
*/
HB_FUNC( CACHEDBRLOCKLIST )
{
   h_database  db   = _db[ hb_parni( 1 ) ];
   char        arg1_ref_val[ CACHE_MAX_BYTES ];
   const int   arg1_ref_cap = CACHE_MAX_BYTES;
   byte_size_t arg1_ref_size;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint(2), 0 );
      cbind_set_next_arg_as_res( db, CBIND_STRING_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddDbrLockList" ) );
      cbind_get_arg_as_str( db, 1, arg1_ref_val, arg1_ref_cap, MULTIBYTE, &arg1_ref_size, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retclen( arg1_ref_val, arg1_ref_size );
}


/* CacheDelete( nConxn, nWA, nRecNo ) => nStatus
*/
HB_FUNC( CACHEDELETE )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db,  hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddDelete" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/* CacheDeleted( nConxn, nWA, nRecNo ) => nStatus
*/
HB_FUNC( CACHEDELETED )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db,  hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddIsRecDeleted" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retni( int_res );
}


/* CacheZap( nConxn, nWA )  => lStatus
*/
HB_FUNC( CACHEZAP )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddZap" ) );
      cbind_get_arg_as_int( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/* CachrRecCount( nConxn, nWA ) => nNumOfRecsInTable
*/
HB_FUNC( CACHERECCOUNT )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddRecCount" ) );
      cbind_get_arg_as_double( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheLastRecordID( nConxn, nWA ) => nLastRecIdOnTableStorage
*/
HB_FUNC( CACHELASTRECORDID )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddLastRecordId" ) );
      cbind_get_arg_as_double( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheOrdKeyno( nConxn, nWA )
*/
HB_FUNC( CACHEORDKEYNO )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddOrdKeyNo" ) );
      cbind_get_arg_as_double( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheOrdKeyCount( nConxn, nWA )
*/
HB_FUNC( CACHEORDKEYCOUNT )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   double      int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_DOUBLE_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddOrdKeyCount" ) );
      cbind_get_arg_as_double( db, 1, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retnint( int_res );
}


/* CacheGoto( nConxn, nWA, nRecNo )
*/
HB_FUNC( CACHEGOTO )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db,  hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_double( db,  hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddOrdGoto" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


/* CacheAppendRecords( nConxn, nWA, cRecInfo, nRecLen )
*/
HB_FUNC( CACHEAPPENDRECORDS )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   char *      arg3 = ( char * ) hb_parc( 3 );
   bool_t      is_null;
   __hbint     int_res;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint(2), 0 );
      cbind_set_next_arg_as_bin( db, arg3, hb_parni( 4 ), 1 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddAppendRecords" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retni( int_res );
}


/* CacheSetGet( nConxn, nWA, nAction, cCargo )
*/
HB_FUNC( CACHESETGET )
{
   h_database  db   = _db[ hb_parni( 1 ) ];
   char        arg1_ref_val[ CACHE_MAX_BYTES ];
   const int   arg1_ref_cap = CACHE_MAX_BYTES;
   byte_size_t arg1_ref_size;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_int( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 4 ), hb_parclen( 4 ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_STRING_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddSetGet" ) );
      cbind_get_arg_as_str( db, 3, arg1_ref_val, arg1_ref_cap, MULTIBYTE, &arg1_ref_size, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   if( is_null )
   {
      hb_retc( " " );
   }
   else
   {
      hb_retclen( arg1_ref_val, arg1_ref_size );
   }
}


/* CacheDateTime( nConxn, nWA )
*/
HB_FUNC( CACHEDATETIME )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   char        arg1_ref_val[36];
   const int   arg1_ref_cap = 36;
   byte_size_t arg1_ref_size;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_STRING_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddDateTime" ) );
      cbind_get_arg_as_str( db, 1, arg1_ref_val, arg1_ref_cap, MULTIBYTE, &arg1_ref_size, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retclen( arg1_ref_val, arg1_ref_size );
}


/* CacheManageLocks( nConxn, nWA, cDlmLockString )
*/
HB_FUNC( CACHEMANAGELOCKS )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   __hbint     int_res;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_str( db, hb_parc( 3 ), strlen( hb_parc( 3 ) ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddManageLocks" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


HB_FUNC( CACHEJUSTATRIP )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   __hbint     int_res;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_res( db, CBIND_INT_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddJustATrip" ) );
      cbind_get_arg_as_int( db, 2, &int_res, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retl( int_res );
}


HB_FUNC( CACHETSTART )
{
   h_database  db = _db[ hb_parni( 1 ) ];

   hb_retnint( cbind_tstart( db ) );
}


HB_FUNC( CACHETCOMMIT )
{
   h_database  db = _db[ hb_parni( 1 ) ];

   hb_retnint( cbind_tcommit( db ) );
}


HB_FUNC( CACHETROLLBACK )
{
   h_database  db = _db[ hb_parni( 1 ) ];

   hb_retnint( cbind_trollback( db ) );
}


HB_FUNC( CACHETLEVEL )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   int res;

   cbind_tlevel( db, &res );

   hb_retnint( res );
}

//--------------------------------------------------------------------//
//                           SQL Query as a Function
//--------------------------------------------------------------------//

/*  CacheExecQuery( nConxn, nMode, nHandle, cSql, cParam, cTableInfo ) => cInfo
*/
HB_FUNC( CACHEEXECUTEQUERY )
{
#if defined(__XHARBOUR__)
   char* arg3 = ( ISNIL( 4 ) ? TEXT( " " ) : ( char * ) hb_parc( 4 ) );
   char* arg4 = ( ISNIL( 5 ) ? TEXT( " " ) : ( char * ) hb_parc( 5 ) );
   char* arg5 = ( ISNIL( 6 ) ? TEXT( " " ) : ( char * ) hb_parc( 6 ) );
#else
   char* arg3 = ( HB_ISNIL( 4 ) ? TEXT( " " ) : ( char * ) hb_parc( 4 ) );
   char* arg4 = ( HB_ISNIL( 5 ) ? TEXT( " " ) : ( char * ) hb_parc( 5 ) );
   char* arg5 = ( HB_ISNIL( 6 ) ? TEXT( " " ) : ( char * ) hb_parc( 6 ) );
#endif
   h_database  db = _db[ hb_parni( 1 ) ];
   char        arg1_ref_val[ CACHE_MAX_BYTES ];
   const int   arg1_ref_cap = CACHE_MAX_BYTES;
   byte_size_t arg1_ref_size;
   bool_t      is_null;

#ifdef __HB_MT__
   if( hb_threadMutexLock( s_pMtx ) )
#endif
   {
      cbind_reset_args( db );

      cbind_set_next_arg_as_int( db, hb_parnint( 2 ), 0 );
      cbind_set_next_arg_as_int( db, hb_parnint( 3 ), 0 );
      cbind_set_next_arg_as_str( db, arg3, strlen( arg3 ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_str( db, arg4, strlen( arg4 ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_str( db, arg5, strlen( arg5 ), MULTIBYTE, 1 );
      cbind_set_next_arg_as_res( db, CBIND_STRING_ID );

      raise_exception( cbind_run_method( db, -1, L"Vouch.XhbRdd", L"RddExeQuery" ) );
      cbind_get_arg_as_str( db, 5, arg1_ref_val, arg1_ref_cap, MULTIBYTE, &arg1_ref_size, &is_null );

#ifdef __HB_MT__
      hb_threadMutexUnlock( s_pMtx );
#endif
   }
   hb_retclen( arg1_ref_val, arg1_ref_size );
}

//----------------------------------------------------------------------//
//                   Q U E R Y   M A N A G E M E N T
//----------------------------------------------------------------------//

/*  CacheOpenQuery( nConxn )
*/
HB_FUNC( CACHEQUERYOPEN )
{
   h_database  db = _db[ hb_parni( 1 ) ];
   h_query     query;
   __hbint     i;
   __hbint     iConxn = 0;
   bool        bOk;

   __hbint ii = cbind_alloc_query( db, &query );
   if( ii == 0 )
   {
      bOk = FALSE;
      for( i=1; i<MAX_QUERIES; i++ )
      {
         if( _qry[ i ] == ( int ) NULL )
         {
            bOk = TRUE;
            break;
         }
      }
      if( bOk )
      {
         _qry[ i ] = query;
         iConxn = i;
      }
   }
   else
   {
      OutputDebugString( ii < 0 ? "cbind_alloc_query < 0" : "cbind_alloc_query 0" );
   }
   hb_retnint( iConxn );
}


HB_FUNC( CACHEQUERYFREE )
{
   h_query query = _qry[ hb_parni( 1 ) ];

   hb_retnint( cbind_free_query( query ) );

   _qry[ hb_parni( 1 ) ] = ( int ) NULL;
}


HB_FUNC( CACHEQUERYPREPARE )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     sql_code;
   WORD    sql_wquery[ 10000 ];

   MultiByteToWideChar( CP_ACP, 0, hb_parc( 2 ), -1, (wchar_t*)sql_wquery, 10000 );

   cbind_prepare_gen_query( query, (const wchar_t*)sql_wquery, &sql_code );

   hb_retnint( sql_code );
}


HB_FUNC( CACHEQUERYGETNUMPARAS )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     paras;

   cbind_query_get_num_pars( query, &paras );

   hb_retnint( paras );
}


HB_FUNC( CACHEQUERYGETPARSQLTYPE )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     type;

   cbind_query_get_par_sql_type( query, hb_parnint( 2 ), &type );

   hb_retnint( type );
}


HB_FUNC( CACHEQUERYGETPARCOLSIZE )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     size;

   cbind_query_get_par_col_size( query, hb_parnint( 2 ), &size );

   hb_retnint( size );
}


HB_FUNC( CACHEQUERYGETNUMCOLS )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     cols;

   cbind_query_get_num_cols( query, &cols );

   hb_retnint( cols );
}


HB_FUNC( CACHEQUERYGETCOLSQLTYPE )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     type;

   cbind_query_get_col_sql_type( query, hb_parnint( 2 ), &type );

   hb_retnint( type );
}


HB_FUNC( CACHEQUERYGETCOLNAME )
{
   h_query       query = _qry[ hb_parni( 1 ) ];
   char          name[MAX_PATH];
   int           idx = hb_parnint( 2 );
   const wchar_t *res;

   cbind_query_get_col_name( query, idx, &res );

   WideCharToMultiByte( CP_ACP, 0, ( const wchar_t* )res, -1, ( char* )&name, MAX_PATH, NULL, false );//&is_null );

   hb_retc( ( char*) name );
}


HB_FUNC( CACHEQUERYGETCOLNAMELEN )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     len;

   cbind_query_get_col_name_len( query, hb_parnint( 2 ), &len );

   hb_retnint( len );
}


HB_FUNC( CACHEQUERYEXECUTE )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     sql_code;

   cbind_query_execute( query, &sql_code );

   hb_retl( !sql_code );
}


HB_FUNC( CACHEQUERYCLOSE )
{
   h_query query = _qry[ hb_parni( 1 ) ];

   hb_retnint( cbind_query_close( query ) );
}


HB_FUNC( CACHEQUERYNEXT )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     sql_code;

   cbind_query_fetch( query, &sql_code );

   hb_retl( !sql_code );
}


HB_FUNC( CACHEQUERYSKIP )
{
   h_query query = _qry[ hb_parni( 1 ) ];

   hb_retl( cbind_query_skip( query, hb_parnint( 2 ) ) );
}


HB_FUNC( CACHEQUERYGETCURIDX )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     curcol;

   cbind_query_get_cur_idx( query, &curcol );

   hb_retnint( curcol );
}


HB_FUNC( CACHEQUERYSETPARAM )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     type = hb_parnint( 2 );
   int     idx  = hb_parnint( 3 );
   int     res = 0;

   switch ( type )
   {
   case  2:
      {
         res = cbind_query_set_double_par( query, idx, hb_parnd( 4 ) );
      }
      break;
   case 12:
      {
         res = cbind_query_set_mb_str_par( query, idx, ( char *) hb_parc( 4 ), strlen( hb_parc( 4 ) ) );
      }
      break;
   case  9:
      {
         res = cbind_query_set_date_par( query, idx, hb_parnint( 4 ), hb_parnint( 5 ), hb_parnint( 6 ) );
      }
      break;
   case -7:
      {
         res = cbind_query_set_int_par( query, idx, ( hb_parl( 4 ) ? 1 : 0 ) );
      }
      break;
   case  4:
      {
         res = cbind_query_set_int_par( query, idx, hb_parnint( 4 ) );
      }
      break;
   }

   hb_retl( res );
}


HB_FUNC( CACHEQUERYGETPARAM )
{
   h_query query = _qry[ hb_parni( 1 ) ];
   int     type = hb_parnint( 2 );
   bool_t  is_null;

   switch ( type )
   {
   case  4:     // integer
      {
         int data;
         cbind_query_get_int_data( query, &data, &is_null );
         hb_retnint( data );
      }
      break;
   case  2:     // Numeric/Double/Float/Real etc
   case  3:
   case  5:
   case  6:
   case  7:
   case  8:
      {
         double data;
         cbind_query_get_double_data( query, &data, &is_null );
         hb_retnd( data );
      }
      break;
   case 12:
      {
         char data[ 32500 ];
         int  psize;
         cbind_query_get_mb_str_data( query, ( char *) &data, 32500, &psize, &is_null );
         hb_retclen( data, psize );
      }
      break;
   case  9:
      {
         int year, month, day;

         PHB_ITEM  info = hb_itemArrayNew( 3 );
         PHB_ITEM  temp = hb_itemNew( NULL );

         cbind_query_get_date_data( query, &year, &month, &day, &is_null );

         hb_arraySet( info, 1, hb_itemPutNI( temp, year  ) );
         hb_arraySet( info, 2, hb_itemPutNI( temp, month ) );
         hb_arraySet( info, 3, hb_itemPutNI( temp, day   ) );

         hb_itemRelease( temp );
         hb_itemReturn( info );
         hb_itemRelease( info );
      }
      break;
   case -7:
      {
         int data;
         cbind_query_get_int_data( query, &data, &is_null );
         hb_retl( data );
      }
      break;
   }

}

//--------------------------------------------------------------------//
// In-Process Table Management -  Tried but could not get them working
//--------------------------------------------------------------------//

HB_FUNC( CACHECLASSOPEN )
{
   h_database   db = _db[ hb_parni( 1 ) ];
   int          iOk;
   WORD         ClassName[MAX_PATH];

   MultiByteToWideChar(CP_ACP, 0, hb_parc( 2 ), -1, ( wchar_t* ) ClassName, MAX_PATH);

   cbind_set_ignore_null(0);

   iOk = cbind_alloc_class_def( db, (const wchar_t*)ClassName, &_cls );

   if( iOk != 0 )
   {
   }
   else
   {
      //_cls = cls_;
   }
   //hb_retnl( (long) _cls );
   hb_retnint( iOk );
}


HB_FUNC( CACHECLASSFREE )
{

   h_database db = _db[ hb_parni( 1 ) ];

   hb_retnint( cbind_free_class_def( db, _cls ) );
}


HB_FUNC( CACHEMETHODOPEN )
{
   cbind_alloc_mtd_def( &_mtd ) ;
   hb_retnl( ( long ) _mtd );
}


HB_FUNC( CACHEMETHODFREE )
{
   hb_retnint( cbind_free_mtd_def( _mtd ) );
}


HB_FUNC( CACHEMETHODGET )
{
   int        iOk;
   WORD       wclass_str[ MAX_PATH ];

   MultiByteToWideChar( CP_ACP, 0, hb_parc( 2 ), -1, ( wchar_t* ) wclass_str, MAX_PATH );
   iOk = cbind_get_mtd_def( _cls, ( const wchar_t* ) wclass_str, _mtd );

   hb_retnint( iOk );
}


HB_FUNC( CACHEPROPERTYGET )
{
   WORD       wclass_str[ MAX_PATH ];

   MultiByteToWideChar( CP_ACP, 0, hb_parc( 2 ), -1, ( wchar_t* ) wclass_str, MAX_PATH );
   cbind_alloc_mtd_def( &_prp ) ;
   cbind_get_prop_def( _cls, ( const wchar_t* ) wclass_str, _prp );

   hb_retnl( ( long ) _prp );
}


HB_FUNC( CACHEGETPROPERTYNAME )
{
   char          name[ MAX_PATH ];
   const wchar_t *res;

   cbind_get_prop_name( ( h_prop_def ) hb_parnl( 1 ), &res );
   WideCharToMultiByte( CP_ACP, 0, ( const wchar_t* )res, -1, ( char* )&name, MAX_PATH, NULL, false );

   hb_retc( ( char* ) name );
}


HB_FUNC( CACHEGETPROPERTYTYPE )
{
   char          name[ MAX_PATH ];
   const wchar_t *res;

   cbind_get_prop_cache_type( ( h_prop_def ) hb_parnl( 1 ), &res );

   WideCharToMultiByte( CP_ACP, 0, ( const wchar_t* ) res, -1, ( char* ) &name, MAX_PATH, NULL, false );

   hb_retc( ( char* ) name );
}


//--------------------------------------------------------------------//
//                             TOBEDONE
//--------------------------------------------------------------------//
/*
   int cbind_alloc_class_def(h_database db,
                             const wchar_t* name,
                             h_class_def* res);
   int cbind_free_class_def(h_database db, h_class_def cl_def);

   int cbind_alloc_prop_def(h_prop_def* res);
   int cbind_free_prop_def(h_prop_def prop_def);

   int cbind_alloc_mtd_def(h_mtd_def *res);
   int cbind_free_mtd_def(h_mtd_def mtd_def);

   int cbind_alloc_arg_def(h_arg_def* res);
   int cbind_free_arg_def(h_arg_def arg_def);

   int cbind_get_prop_def(h_class_def cl_def,
                          const wchar_t* name,
                          h_prop_def res);
   int cbind_get_dyn_prop_def(h_database db,
                              h_objref oref,
                              const wchar_t* name,
                              h_prop_def res);

   int cbind_get_prop_cpp_type(h_prop_def h_def, short *val);
   int cbind_get_prop_cache_type(h_prop_def h_def, const wchar_t **val);
   int cbind_get_prop_name(h_prop_def h_def, const wchar_t **val);

   int cbind_get_mtd_def(h_class_def cl_def,
                         const wchar_t* name,
                         h_mtd_def h_def);
   int cbind_get_dyn_mtd_def(h_database db,
                             h_objref oref,
                             const wchar_t* name,
                             h_mtd_def res);

   int cbind_get_mtd_is_func(h_mtd_def h_def, bool_t *is_func);
   int cbind_get_mtd_cpp_type(h_mtd_def h_def, short *cpp_type);
   int cbind_get_mtd_cache_type(h_mtd_def h_def, const wchar_t** cache_type);
   int cbind_get_mtd_is_cls_mtd(h_mtd_def h_def, bool_t *is_cls_mtd);
   int cbind_get_mtd_num_args(h_mtd_def h_def, int *num_args);
   int cbind_get_mtd_args_info(h_mtd_def h_def, void ** args_info);
   int cbind_get_mtd_name(h_mtd_def h_def, const wchar_t **name);

   int cbind_get_mtd_info(h_mtd_def h_def, int option, void *p_info);  // p_info is address of variable to hold returned information

   int cbind_mtd_rewind_args(h_mtd_def mtd_def);
   int cbind_get_arg_cpp_type(h_arg_def h_def, short *cpp_type);
   int cbind_get_arg_cache_type(h_arg_def h_def, const wchar_t **cache_type);
   int cbind_get_arg_name(h_arg_def h_def, const wchar_t **name);
   int cbind_get_arg_is_by_ref(h_arg_def h_def, bool_t *is_by_ref);
   int cbind_get_arg_is_default(h_arg_def h_def, bool_t *is_default);
   int cbind_get_arg_def_val(h_arg_def h_def, const char **def_val);
   int cbind_get_arg_def_val_size(h_arg_def h_def, long *def_val_size);

   int cbind_mtd_arg_get(h_mtd_def mtd_def, h_arg_def arg_def);
   int cbind_mtd_arg_next(h_mtd_def mtd_def);

   int cbind_get_next_prop_def(h_class_def h_cl_def, h_prop_def prop_def, bool_t *p_at_end);
   int cbind_get_next_mtd_def(h_class_def h_cl_def, h_mtd_def mtd_def, bool_t *p_at_end);

   int cbind_reset_prop_defs(h_class_def h_cl_def);
   int cbind_reset_mtd_defs(h_class_def h_cl_def);
*/
