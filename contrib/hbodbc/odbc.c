/*
 * This file contains source for first ODBC routines.
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 * Copyright 2006 Marcelo Lombardo <lombardo@uol.com.br>
 *   (SQLGetInfo(), SQLSetStmtAttr(), SQLGetStmtAttr(), SQLCommit(),
 *   SQLRollback(), SQLColAttribute(), SQLMoreResults())
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
 *   (SQLNumResultCols(), SQLDescribeCol())
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapistr.h"
#include "hbdate.h"
#include "hbset.h"

/* NOTE: This code using pointer items is a little bit more complicated
         then it has to be.
         In current code pointer items can keep references only to other
         pointer items in the form which cannot create cyclic reference.
         They also do not have any references to codeblocks, arrays,
         objects, hashes or any other complex items set by user which
         may create indirectly cyclic reference. In such case it's
         possible to eliminate all GC mark functions and use directly
         items allocated by hb_itemNew() without unlocking them with
         hb_gcUnlock(). By default all items allocated by hb_itemNew()
         are locked so GC automatically mark them and their GC memory
         blocks as used.
         It does not cause any speed or memory overhead so I left it
         as example for more complicated cases and as base for some
         potential extensions. [druzus] */

/* Required by headers on Windows */
#if defined( HB_OS_WIN )

   /* NOTE: Workaround for OpenWatcom's (tested with 1.9) odbc32.lib implib
            missing the entry for wide version of one function,
            so we turn off UNICODE, until it's fixed in OpenWatcom:
               Error! E2028: _SQLSetStmtAttrW@16 is an undefined reference
            [vszakats] */
   #if defined( __WATCOMC__ ) && defined( UNICODE )
      #undef UNICODE
   #endif

   #include <windows.h>

   #if ! defined( WIN32 )
      #define WIN32  /* Required for WIN32_LEAN_AND_MEAN mode */
   #endif
#endif

#include <sql.h>
#include <sqlext.h>

#if ! defined( HB_OS_WIN )
   #if ! defined( SQLLEN ) && ! defined( SQLTCHAR ) && \
       ! defined( UODBCINT64 ) && ! defined( SIZEOF_LONG_INT )
      typedef unsigned char SQLTCHAR;
      typedef long          SQLLEN;
      typedef unsigned long SQLULEN;
      #ifndef SQL_WCHAR
         #define SQL_WCHAR         ( -8 )
      #endif
      #ifndef SQL_WVARCHAR
         #define SQL_WVARCHAR      ( -9 )
      #endif
      #ifndef SQL_WLONGVARCHAR
         #define SQL_WLONGVARCHAR  ( -10 )
      #endif
   #endif
#endif

#ifndef SQL_NO_DATA
#define SQL_NO_DATA  SQL_NO_DATA_FOUND
#endif

#if defined( UNICODE )
   #define O_HB_PARSTR( n, h, len )                 hb_parstr_u16( n, HB_CDP_ENDIAN_NATIVE, h, len )
   #define O_HB_PARSTRDEF( n, h, len )              hb_wstrnull( hb_parstr_u16( n, HB_CDP_ENDIAN_NATIVE, h, len ) )
   #define O_HB_STORSTR( str, n )                   hb_storstr_u16( HB_CDP_ENDIAN_NATIVE, str, n )
   #define O_HB_STORSTRLEN( str, len, n )           hb_storstrlen_u16( HB_CDP_ENDIAN_NATIVE, str, len, n )
   #define O_HB_ARRAYGETSTR( arr, n, phstr, plen )  hb_arrayGetStrU16( arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define O_HB_ITEMCOPYSTR( itm, str, len )        hb_itemCopyStrU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define O_HB_ITEMGETSTR( itm, phstr, plen )      hb_itemGetStrU16( itm, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define O_HB_ITEMPUTSTR( itm, str )              hb_itemPutStrU16( itm, HB_CDP_ENDIAN_NATIVE, str )
   #define O_HB_ITEMPUTSTRLEN( itm, str, len )      hb_itemPutStrLenU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define O_HB_CHAR  HB_WCHAR
#else
   #define O_HB_PARSTR( n, h, len )                 hb_parstr( n, hb_setGetOSCP(), h, len )
   #define O_HB_PARSTRDEF( n, h, len )              hb_strnull( hb_parstr( n, hb_setGetOSCP(), h, len ) )
   #define O_HB_STORSTR( str, n )                   hb_storstr( hb_setGetOSCP(), str, n )
   #define O_HB_STORSTRLEN( str, len, n )           hb_storstrlen( hb_setGetOSCP(), str, len, n )
   #define O_HB_ARRAYGETSTR( arr, n, phstr, plen )  hb_arrayGetStr( arr, n, hb_setGetOSCP(), phstr, plen )
   #define O_HB_ITEMCOPYSTR( itm, str, len )        hb_itemCopyStr( itm, hb_setGetOSCP(), str, len )
   #define O_HB_ITEMGETSTR( itm, phstr, plen )      hb_itemGetStr( itm, hb_setGetOSCP(), phstr, plen )
   #define O_HB_ITEMPUTSTR( itm, str )              hb_itemPutStr( itm, hb_setGetOSCP(), str )
   #define O_HB_ITEMPUTSTRLEN( itm, str, len )      hb_itemPutStrLen( itm, hb_setGetOSCP(), str, len )
   #define O_HB_CHAR  char
#endif

/* GC - SQLHENV */

typedef struct
{
   SQLHENV  hEnv;
}
HB_SQLHENV, * PHB_SQLHENV;

static HB_GARBAGE_FUNC( hb_SQLHENV_Destructor )
{
   PHB_SQLHENV pHEnv = ( PHB_SQLHENV ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( pHEnv->hEnv )
   {
#if ODBCVER >= 0x0300
      SQLFreeHandle( SQL_HANDLE_ENV, ( SQLHANDLE ) pHEnv->hEnv );
#else
      SQLFreeEnv( pHEnv->hEnv );
#endif

      /* set pointer to NULL to avoid multiple freeing */
      pHEnv->hEnv = NULL;
   }
}

static const HB_GC_FUNCS s_gcSQLHENVFuncs =
{
   hb_SQLHENV_Destructor,
   hb_gcDummyMark
};

static void hb_SQLHENV_stor( SQLHENV hEnv, int iParam )
{
   PHB_SQLHENV pHEnv = ( PHB_SQLHENV ) hb_gcAllocate( sizeof( HB_SQLHENV ), &s_gcSQLHENVFuncs );

   pHEnv->hEnv = hEnv;

   hb_storptrGC( ( void * ) pHEnv, iParam );
}

static SQLHENV hb_SQLHENV_par( int iParam )
{
   PHB_SQLHENV pHEnv = ( PHB_SQLHENV ) hb_parptrGC( &s_gcSQLHENVFuncs, iParam );

   return pHEnv ? pHEnv->hEnv : NULL;
}

/* GC - SQLHDBC */

typedef struct
{
   SQLHDBC  hDbc;
   PHB_ITEM pHEnvItm;
   int      conn_counter;
}
HB_SQLHDBC, * PHB_SQLHDBC;

static HB_GARBAGE_FUNC( hb_SQLHDBC_Destructor )
{
   /* Retrieve image pointer holder */
   PHB_SQLHDBC pHDbc = ( PHB_SQLHDBC ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( pHDbc->hDbc )
   {
#if ODBCVER >= 0x0300
      SQLFreeHandle( SQL_HANDLE_DBC, ( SQLHANDLE ) pHDbc->hDbc );
#else
      SQLFreeConnect( pHDbc->hDbc );
#endif

      /* set pointer to NULL to avoid multiple freeing */
      pHDbc->hDbc = NULL;
   }
   if( pHDbc->pHEnvItm )
   {
      /* release reference to parent handler */
      hb_itemRelease( pHDbc->pHEnvItm );
      /* set pointer to NULL to avoid multiple freeing */
      pHDbc->pHEnvItm = NULL;
   }
}

static HB_GARBAGE_FUNC( hb_SQLHDBC_Mark )
{
   /* Retrieve image pointer holder */
   PHB_SQLHDBC pHDbc = ( PHB_SQLHDBC ) Cargo;

   if( pHDbc->pHEnvItm )
      /* mark parent item handler as used */
      hb_gcMark( pHDbc->pHEnvItm );
}

static const HB_GC_FUNCS s_gcSQLHDBCFuncs =
{
   hb_SQLHDBC_Destructor,
   hb_SQLHDBC_Mark
};

static void hb_SQLHDBC_stor( PHB_ITEM pHEnvItm, SQLHDBC hDbc, int iParam )
{
   PHB_SQLHDBC pHDbc = ( PHB_SQLHDBC ) hb_gcAllocate( sizeof( HB_SQLHDBC ), &s_gcSQLHDBCFuncs );

   pHDbc->hDbc = hDbc;
   pHDbc->conn_counter = 1;
   /* initialize pointer scanned by mark function before allocating new
      new GC block - such allocation may activate GC and uninitialized
      pointer will be accessed from our mark function */
   pHDbc->pHEnvItm = NULL;
   if( pHEnvItm )
   {
      pHDbc->pHEnvItm = hb_itemNew( pHEnvItm );
      hb_gcUnlock( pHDbc->pHEnvItm );
   }

   hb_storptrGC( ( void * ) pHDbc, iParam );
}

static PHB_SQLHDBC hb_SQLHDBC_get( PHB_ITEM pItem )
{
   return ( PHB_SQLHDBC ) hb_itemGetPtrGC( pItem, &s_gcSQLHDBCFuncs );
}

static HB_BOOL hb_SQLHDBC_check( PHB_ITEM pItem, int conn_counter )
{
   if( pItem )
   {
      PHB_SQLHDBC pHDbc = hb_SQLHDBC_get( pItem );

      return pHDbc != NULL && pHDbc->conn_counter == conn_counter;
   }
   else
      return HB_TRUE;
}

static SQLHDBC hb_SQLHDBC_par( int iParam )
{
   PHB_SQLHDBC pHDbc = hb_SQLHDBC_get( hb_param( iParam, HB_IT_POINTER ) );

   return pHDbc ? pHDbc->hDbc : NULL;
}

/* GC - SQLHSTMT */

typedef struct
{
   SQLHSTMT hStmt;
   PHB_ITEM pHDbcItm;
   int      conn_counter;
}
HB_SQLHSTMT, * PHB_SQLHSTMT;

static HB_GARBAGE_FUNC( hb_SQLHSTMT_Destructor )
{
   /* Retrieve image pointer holder */
   PHB_SQLHSTMT pHStmt = ( PHB_SQLHSTMT ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( pHStmt->hStmt )
   {
      if( hb_SQLHDBC_check(  pHStmt->pHDbcItm, pHStmt->conn_counter ) )
      {
#if ODBCVER >= 0x0300
         SQLFreeHandle( SQL_HANDLE_STMT, ( SQLHANDLE ) pHStmt->hStmt );
#else
         SQLFreeStmt( pHStmt->hStmt, SQL_DROP );
#endif
      }

      /* set pointer to NULL to avoid multiple freeing */
      pHStmt->hStmt = NULL;
   }

   if( pHStmt->pHDbcItm )
   {
      /* release reference to parent handler */
      hb_itemRelease( pHStmt->pHDbcItm );
      /* set pointer to NULL to avoid multiple freeing */
      pHStmt->pHDbcItm = NULL;
   }
}

static HB_GARBAGE_FUNC( hb_SQLHSTMT_Mark )
{
   /* Retrieve image pointer holder */
   PHB_SQLHSTMT pHStmt = ( PHB_SQLHSTMT ) Cargo;

   if( pHStmt->pHDbcItm )
      /* mark parent item handler as used */
      hb_gcMark( pHStmt->pHDbcItm );
}
static const HB_GC_FUNCS s_gcSQLHSTMTFuncs =
{
   hb_SQLHSTMT_Destructor,
   hb_SQLHSTMT_Mark
};

static void hb_SQLHSTMT_stor( PHB_ITEM pHDbcItm, SQLHSTMT hStmt, int iParam )
{
   PHB_SQLHSTMT pHStmt = ( PHB_SQLHSTMT ) hb_gcAllocate( sizeof( HB_SQLHSTMT ), &s_gcSQLHSTMTFuncs );

   pHStmt->hStmt = hStmt;
   pHStmt->conn_counter = 0;
   /* initialize pointer scanned by mark function before allocating new
      new GC block - such allocation may activate GC and uninitalized
      pointer will be accessed from our mark function */
   pHStmt->pHDbcItm = NULL;

   if( pHDbcItm )
   {
      PHB_SQLHDBC pHDbc = hb_SQLHDBC_get( pHDbcItm );

      if( pHDbc )
      {
         pHStmt->conn_counter = pHDbc->conn_counter;
         pHStmt->pHDbcItm = hb_itemNew( pHDbcItm );
         hb_gcUnlock( pHStmt->pHDbcItm );
      }
   }

   hb_storptrGC( ( void * ) pHStmt, iParam );
}

static SQLHSTMT hb_SQLHSTMT_par( int iParam )
{
   PHB_SQLHSTMT pHStmt = ( PHB_SQLHSTMT ) hb_parptrGC( &s_gcSQLHSTMTFuncs, iParam );

   return ( pHStmt && pHStmt->hStmt &&
            hb_SQLHDBC_check(  pHStmt->pHDbcItm, pHStmt->conn_counter ) ) ?
          pHStmt->hStmt : NULL;
}

HB_FUNC( SQLALLOCENV )  /* @hEnv --> nRetCode */
{
   SQLHENV   hEnv;
   SQLRETURN result;

#if ODBCVER >= 0x0300
   result = SQLAllocHandle( SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv );

   if( SQL_SUCCEEDED( result ) )
      SQLSetEnvAttr( hEnv, SQL_ATTR_ODBC_VERSION, ( SQLPOINTER ) SQL_OV_ODBC3, SQL_IS_UINTEGER );
#else
   result = SQLAllocEnv( &hEnv );
#endif

   hb_retni( result );

   hb_SQLHENV_stor( hEnv, 1 );
}

HB_FUNC( SQLALLOCCONNECT )  /* hEnv, @hDbc --> nRetCode */
{
   SQLHENV hEnv = hb_SQLHENV_par( 1 );

   if( hEnv )
   {
      SQLHDBC hDbc;

#if ODBCVER >= 0x0300
      hb_retni( SQLAllocHandle( SQL_HANDLE_DBC, hEnv, &hDbc ) );
#else
      hb_retni( SQLAllocConnect( hEnv, &hDbc ) );
#endif

      hb_SQLHDBC_stor( hb_param( 1, HB_IT_ANY ), hDbc, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLDRIVERCONNECT )  /* hDbc, @cConnectString --> nRetCode */
{
   SQLHDBC hDbc = hb_SQLHDBC_par( 1 );

   if( hDbc )
   {
      SQLSMALLINT iLen;
      SQLRETURN   ret;
      void *      hConnStr;
      HB_SIZE     nConnStr;
      SQLTCHAR *  cConnStr = ( SQLTCHAR * ) HB_UNCONST( O_HB_PARSTRDEF( 2, &hConnStr, &nConnStr ) );
      SQLTCHAR    buffer[ 1024 ];

      buffer[ 0 ] = '\0';

      ret = SQLDriverConnect( hDbc,
                              ( SQLHWND ) NULL,
                              cConnStr,
                              ( SQLSMALLINT ) nConnStr,
                              ( SQLTCHAR * ) buffer,
                              ( SQLSMALLINT ) HB_SIZEOFARRAY( buffer ),
                              ( SQLSMALLINT * ) &iLen,
                              ( SQLUSMALLINT ) SQL_DRIVER_COMPLETE );

      hb_strfree( hConnStr );

      O_HB_STORSTR( ( O_HB_CHAR * ) buffer, 3 );

      hb_retni( ret );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLCONNECT )  /* hDbc, cDSN, cUseName, cPassword --> nRetCode */
{
   SQLHDBC hDbc = hb_SQLHDBC_par( 1 );

   /* FIXME: add protection against connection with connected hDbc handle */

   if( hDbc )
   {
      SQLRETURN ret;

      void *     hDSN;
      void *     hUser;
      void *     hPass;
      HB_SIZE    nDSN, nUser, nPass;
      SQLTCHAR * cDSN  = ( SQLTCHAR * ) HB_UNCONST( O_HB_PARSTRDEF( 2, &hDSN, &nDSN ) );
      SQLTCHAR * cUser = ( SQLTCHAR * ) HB_UNCONST( O_HB_PARSTRDEF( 3, &hUser, &nUser ) );
      SQLTCHAR * cPass = ( SQLTCHAR * ) HB_UNCONST( O_HB_PARSTRDEF( 4, &hPass, &nPass ) );

      ret = SQLConnect( hDbc,
                        cDSN,
                        ( SQLSMALLINT ) nDSN,
                        cUser,
                        ( SQLSMALLINT ) nUser,
                        cPass,
                        ( SQLSMALLINT ) nPass );

      hb_strfree( hDSN );
      hb_strfree( hUser );
      hb_strfree( hPass );

      hb_retni( ret );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLDISCONNECT )  /* hDbc --> nRetCode */
{
   PHB_SQLHDBC pHDbc = hb_SQLHDBC_get( hb_param( 1, HB_IT_POINTER ) );

   if( pHDbc && pHDbc->hDbc )
   {
      SQLRETURN result = SQLDisconnect( pHDbc->hDbc );

      if( SQL_SUCCEEDED( result ) )
         pHDbc->conn_counter++;

      hb_retni( result );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if defined( HB_LEGACY_LEVEL4 )

HB_FUNC( SQLFREECONNECT )  /* hDbc --> nRetCode */
{
}

HB_FUNC( SQLFREEENV )  /* hEnv --> nRetCode */
{
}

HB_FUNC( SQLFREESTMT )  /* hStmt, nType --> nRetCode */
{
}

#endif

HB_FUNC( SQLALLOCSTMT )  /* hDbc, @hStmt --> nRetCode */
{
   SQLHDBC hDbc = hb_SQLHDBC_par( 1 );

   if( hDbc )
   {
      SQLHSTMT hStmt;

#if ODBCVER >= 0x0300
      hb_retni( SQLAllocHandle( SQL_HANDLE_STMT, hDbc, &hStmt ) );
#else
      hb_retni( SQLAllocStmt( hDbc, &hStmt ) );
#endif

      hb_SQLHSTMT_stor( hb_param( 1, HB_IT_ANY ), hStmt, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLEXECDIRECT )  /* hStmt, cStatement --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
      void *     hStatement;
      HB_SIZE    nStatement;
      SQLTCHAR * cStatement = ( SQLTCHAR * ) HB_UNCONST( O_HB_PARSTRDEF( 2, &hStatement, &nStatement ) );

      hb_retni( SQLExecDirect( hStmt,
                               cStatement,
                               ( SQLINTEGER ) nStatement ) );
      hb_strfree( hStatement );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLFETCH )  /* hStmt --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
      hb_retni( SQLFetch( hStmt ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLFETCHSCROLL )
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
#if ODBCVER >= 0x0300
      hb_retni( SQLFetchScroll( hStmt,
                                ( SQLSMALLINT ) hb_parni( 2 ),
                                ( SQLLEN ) hb_parnint( 3 ) ) );
#else
      hb_retni( SQL_ERROR );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETDATA )  /* hStmt, nField, nType, [nMaxLen], @xValue --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
      SQLUSMALLINT uiField = ( SQLUSMALLINT ) hb_parni( 2 );
      SQLSMALLINT  iType   = ( SQLSMALLINT ) hb_parnidef( 3, SQL_BINARY );
      SQLLEN       nLen    = 0;
      SQLRETURN    res     = SQL_ERROR;

      switch( iType )
      {
         case SQL_CHAR:
         case SQL_VARCHAR:
         case SQL_LONGVARCHAR:
         case SQL_WCHAR:
         case SQL_WVARCHAR:
         case SQL_WLONGVARCHAR:
         {
            O_HB_CHAR * val = NULL;
            O_HB_CHAR buffer[ 1 ];
#if defined( UNICODE )
            SQLSMALLINT iTargetType = SQL_C_WCHAR;
#else
            SQLSMALLINT iTargetType = SQL_C_CHAR;
#endif

            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, iTargetType, buffer, 0, &nLen ) ) )
            {
               if( nLen > 0 )
               {
                  SQLLEN nMaxLen = ( SQLLEN ) hb_parnint( 4 );

#if defined( UNICODE )
                  nMaxLen <<= 1;
#endif
                  if( nMaxLen > 0 && nMaxLen < nLen )
                     nLen = nMaxLen;
                  val = ( O_HB_CHAR * ) hb_xgrab( nLen + sizeof( O_HB_CHAR ) );
                  if( ! SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, iTargetType, val, nLen + sizeof( O_HB_CHAR ), &nLen ) ) )
                  {
                     hb_xfree( val );
                     val = NULL;
                  }
#if defined( UNICODE )
                  else
                     nLen >>= 1;
#endif
               }
            }
            if( val != NULL )
            {
               O_HB_STORSTRLEN( val, ( HB_SIZE ) nLen, 5 );
               hb_xfree( val );
            }
            else
               hb_storc( NULL, 5 );
            break;
         }

         case SQL_BINARY:
         case SQL_VARBINARY:
         case SQL_LONGVARBINARY:
         {
            char * val = NULL;
            char buffer[ 1 ];

            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_BINARY, buffer, 0, &nLen ) ) )
            {
               if( nLen > 0 )
               {
                  SQLLEN nMaxLen = ( SQLLEN ) hb_parnint( 4 );

                  if( nMaxLen > 0 && nMaxLen < nLen )
                     nLen = nMaxLen;
                  val = ( char * ) hb_xgrab( nLen + 1 );
                  if( ! SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_BINARY, val, nLen + 1, &nLen ) ) )
                  {
                     hb_xfree( val );
                     val = NULL;
                  }
               }
            }
            if( val )
            {
               if( ! hb_storclen_buffer( val, ( HB_SIZE ) nLen, 5 ) )
                  hb_xfree( val );
            }
            else
               hb_storc( NULL, 5 );
            break;
         }

         case SQL_BIGINT:
#if ODBCVER >= 0x0300
         {
            HB_I64 val = 0;
            /* NOTE: SQL_C_SBIGINT not available before ODBC 3.0 */
            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_SBIGINT, &val, sizeof( val ), &nLen ) ) )
               hb_stornint( val, 5 );
            else
               hb_stornint( 0, 5 );
            break;
         }
#endif
         case SQL_TINYINT:
         case SQL_SMALLINT:
         case SQL_INTEGER:
         {
            SQLINTEGER val = 0;
            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_LONG, &val, sizeof( val ), &nLen ) ) )
               hb_stornint( val, 5 );
            else
               hb_stornint( 0, 5 );
            break;
         }

         case SQL_DECIMAL:
         case SQL_NUMERIC:
         case SQL_REAL:
         case SQL_FLOAT:
         case SQL_DOUBLE:
         {
            double val = 0.0;
            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_DOUBLE, &val, sizeof( val ), &nLen ) ) )
               hb_stornd( val, 5 );
            else
               hb_stornd( 0.0, 5 );
            break;
         }

         case SQL_BIT:
         {
            unsigned char val = 0;
            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_BIT, &val, sizeof( val ), &nLen ) ) )
               hb_storl( val != 0, 5 );
            else
               hb_storl( HB_FALSE, 5 );
            break;
         }

         case SQL_DATE:
#if ODBCVER >= 0x0300
         case SQL_TYPE_DATE:
#endif
         {
            DATE_STRUCT val = { 0, 0, 0 };
            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_DATE, &val, sizeof( val ), &nLen ) ) )
               hb_stordl( hb_dateEncode( val.year, val.month, val.day ), 5 );
            else
               hb_stordl( 0, 5 );
            break;
         }

         case SQL_TIME:
#if ODBCVER >= 0x0300
         case SQL_TYPE_TIME:
#endif
         {
            TIME_STRUCT val = { 0, 0, 0 };
            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_TIME, &val, sizeof( val ), &nLen ) ) )
               hb_stortdt( 0, hb_timeEncode( val.hour, val.minute, val.second, 0 ), 5 );
            else
               hb_stortdt( 0, 0, 5 );
            break;
         }

         case SQL_TIMESTAMP:
#if ODBCVER >= 0x0300
         case SQL_TYPE_TIMESTAMP:
#endif
         {
            TIMESTAMP_STRUCT val = { 0, 0, 0, 0, 0, 0, 0 };
            if( SQL_SUCCEEDED( res = SQLGetData( hStmt, uiField, SQL_C_TIMESTAMP, &val, sizeof( val ), &nLen ) ) )
               hb_stortdt( hb_dateEncode( val.year, val.month, val.day ),
                           hb_timeEncode( val.hour, val.minute, val.second, val.fraction / 1000000 ), 5 );
            else
               hb_stortdt( 0, 0, 5 );
            break;
         }
         default:
            hb_stor( 5 );
      }

      hb_retni( res );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLNUMRESULTCOLS )  /* hStmt, @nColCount --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
      SQLSMALLINT iCols = 0;

      hb_retni( SQLNumResultCols( hStmt, &iCols ) );

      hb_stornl( ( long ) iCols, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLDESCRIBECOL )  /* hStmt, nCol, @cName, nLen, @nBufferLen, @nDataType, @nColSize, @nDec, @nNull --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
      SQLSMALLINT iLen      = ( SQLSMALLINT ) hb_parni( 4 );
      SQLSMALLINT iBufLen   = ( SQLUSMALLINT ) hb_parni( 5 );
      SQLSMALLINT iDataType = ( SQLUSMALLINT ) hb_parni( 6 );
      SQLULEN     nColSize  = ( SQLULEN ) hb_parnint( 7 );
      SQLSMALLINT iDecimals = ( SQLUSMALLINT ) hb_parni( 8 );
      SQLSMALLINT iNullable = ( SQLUSMALLINT ) hb_parni( 9 );
      SQLTCHAR *  buffer;

      if( iLen <= 0 )
         iLen = 64;

      buffer      = ( SQLTCHAR * ) hb_xgrab( iLen * sizeof( SQLTCHAR ) );
      buffer[ 0 ] = '\0';

      hb_retni( SQLDescribeCol( hStmt,
                                ( SQLUSMALLINT ) hb_parni( 2 ),
                                ( SQLTCHAR * ) buffer,
                                ( SQLSMALLINT ) iLen,
                                ( SQLSMALLINT * ) &iBufLen,
                                ( SQLSMALLINT * ) &iDataType,
                                ( SQLULEN * ) &nColSize,
                                ( SQLSMALLINT * ) &iDecimals,
                                ( SQLSMALLINT * ) &iNullable ) );

      O_HB_STORSTRLEN( ( O_HB_CHAR * ) buffer, ( HB_SIZE ) iBufLen, 3 );
      hb_storni( ( int ) iBufLen, 5 );
      hb_storni( ( int ) iDataType, 6 );
      hb_stornint( nColSize, 7 );
      hb_storni( ( int ) iDecimals, 8 );
      hb_storni( ( int ) iNullable, 9 );

      hb_xfree( buffer );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLCOLATTRIBUTE )  /* hStmt, nCol, nField, @cName, nLen, @nBufferLen, @nAttribute --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
      SQLSMALLINT iLen    = ( SQLSMALLINT ) hb_parni( 5 );
      SQLSMALLINT iBufLen = ( SQLUSMALLINT ) hb_parni( 6 );

#if ODBCVER >= 0x0300
      SQLLEN nNumPtr = ( SQLLEN ) hb_parnint( 7 );
#else
      SQLINTEGER nNumPtr = ( SQLINTEGER ) hb_parnl( 7 );
#endif
      char * buffer;

      if( iLen <= 0 )
         iLen = 64;

      buffer      = ( char * ) hb_xgrab( iLen );
      buffer[ 0 ] = '\0';

#if ODBCVER >= 0x0300
      hb_retni( SQLColAttribute( hStmt,
                                 ( SQLUSMALLINT ) hb_parni( 2 ),
                                 ( SQLUSMALLINT ) hb_parni( 3 ),
                                 ( SQLPOINTER ) buffer,
                                 iLen,
                                 ( SQLSMALLINT * ) &iBufLen,
                                 ( SQLLEN * ) &nNumPtr ) );
#else
      hb_retni( SQLColAttributes( hStmt,
                                  ( SQLUSMALLINT ) hb_parni( 2 ),
                                  ( SQLUSMALLINT ) hb_parni( 3 ),
                                  ( SQLPOINTER ) buffer,
                                  iLen,
                                  ( SQLSMALLINT * ) &iBufLen,
                                  ( SQLINTEGER * ) &nNumPtr ) );
#endif

      hb_storclen( buffer, ( HB_SIZE ) iBufLen, 4 );
      hb_storni( ( int ) iBufLen, 6 );
      hb_stornint( nNumPtr, 7 );

      hb_xfree( buffer );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLERROR )  /* hEnv, hDbc, hStmt, @cErrorClass, @nType, @cErrorMsg */
{
   SQLHENV  hEnv  = hb_SQLHENV_par( 1 );
   SQLHDBC  hDbc  = hb_SQLHDBC_par( 2 );
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 3 );

   if( hEnv || hDbc || hStmt )
   {
      SQLINTEGER  lError;
      SQLSMALLINT iLen;
      SQLTCHAR    buffer[ 256 ];
      SQLTCHAR    szErrorMsg[ SQL_MAX_MESSAGE_LENGTH + 1 ];

      buffer[ 0 ]     = '\0';
      szErrorMsg[ 0 ] = '\0';
      iLen = 0;

      hb_retni( SQLError( hEnv,
                          hDbc,
                          hStmt,
                          ( SQLTCHAR * ) buffer,
                          ( SQLINTEGER * ) &lError,
                          ( SQLTCHAR * ) szErrorMsg,
                          ( SQLSMALLINT ) HB_SIZEOFARRAY( szErrorMsg ),
                          ( SQLSMALLINT * ) &iLen ) );

      O_HB_STORSTR( ( O_HB_CHAR * ) buffer, 4 );
      hb_stornl( ( long ) lError, 5 );
      O_HB_STORSTRLEN( ( O_HB_CHAR * ) szErrorMsg, iLen, 6 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETDIAGREC )  /* nHandleType, hHandle, nRecNumber, @cSQLState, @nError, @cErrorMsg */
{
#if ODBCVER >= 0x0300
   SQLSMALLINT iHandleType = ( SQLSMALLINT ) hb_parni( 1 );
   SQLHANDLE   hHandle;

   switch( iHandleType )
   {
      case SQL_HANDLE_DBC:
         hHandle = hb_SQLHDBC_par( 2 );
         break;
      case SQL_HANDLE_ENV:
         hHandle = hb_SQLHENV_par( 2 );
         break;
      case SQL_HANDLE_STMT:
         hHandle = hb_SQLHSTMT_par( 2 );
         break;
      default:
         hHandle = NULL;
   }

   if( hHandle )
   {
      SQLTCHAR    szSQLState[ 5 + 1 ];
      SQLINTEGER  lError;
      SQLTCHAR    szErrorMsg[ SQL_MAX_MESSAGE_LENGTH + 1 ];
      SQLSMALLINT iLen;

      szSQLState[ 0 ] = '\0';
      szErrorMsg[ 0 ] = '\0';
      iLen = 0;

      hb_retni( SQLGetDiagRec( iHandleType,
                               hHandle,
                               ( SQLSMALLINT ) hb_parni( 3 ),
                               ( SQLTCHAR * ) szSQLState,
                               ( SQLINTEGER * ) &lError,
                               ( SQLTCHAR * ) szErrorMsg,
                               ( SQLSMALLINT ) HB_SIZEOFARRAY( szErrorMsg ),
                               ( SQLSMALLINT * ) &iLen ) );

      O_HB_STORSTR( ( O_HB_CHAR * ) szSQLState, 4 );
      hb_stornl( ( long ) lError, 5 );
      O_HB_STORSTRLEN( ( O_HB_CHAR * ) szErrorMsg, iLen, 6 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retni( SQL_ERROR );
   hb_storc( NULL, 4 );
   hb_stornl( 0, 5 );
   hb_storc( NULL, 6 );
#endif
}

HB_FUNC( SQLROWCOUNT )
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
      SQLLEN iRowCountPtr = ( SQLLEN ) hb_parnint( 2 );

      hb_retni( SQLRowCount( hStmt, ( SQLLEN * ) &iRowCountPtr ) );

      hb_stornint( iRowCountPtr, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETINFO )  /* hDbc, nType, @cResult */
{
   SQLHDBC hDbc = hb_SQLHDBC_par( 1 );

   if( hDbc )
   {
      char        buffer[ 512 ];
      SQLSMALLINT iLen = 0;

      buffer[ 0 ] = '\0';

      hb_retni( SQLGetInfo( hDbc,
                            ( SQLUSMALLINT ) hb_parni( 2 ),
                            ( SQLPOINTER ) buffer,
                            ( SQLSMALLINT ) sizeof( buffer ),
                            ( SQLSMALLINT * ) &iLen ) );

      hb_storclen( buffer, iLen, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLSETCONNECTATTR )  /* hDbc, nOption, uOption */
{
   SQLHDBC hDbc = hb_SQLHDBC_par( 1 );

   if( hDbc )
   {
#if ODBCVER >= 0x0300
      hb_retni( SQLSetConnectAttr( hDbc,
                                   ( SQLINTEGER ) hb_parnl( 2 ),
                                   HB_ISCHAR( 3 ) ? ( SQLPOINTER ) HB_UNCONST( hb_parc( 3 ) ) :
                                                    ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                   HB_ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) :
                                                    ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
      hb_retni( SQLSetConnectOption( hDbc,
                                     ( SQLUSMALLINT ) hb_parni( 2 ),
                                     HB_ISCHAR( 3 ) ? ( SQLULEN ) ( HB_PTRUINT ) hb_parc( 3 ) :
                                                      ( SQLULEN ) hb_parnl( 3 ) ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLSETSTMTATTR )  /* hStmt, nOption, uOption --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
#if ODBCVER >= 0x0300
      hb_retni( SQLSetStmtAttr( hStmt,
                                ( SQLINTEGER ) hb_parnl( 2 ),
                                HB_ISCHAR( 3 ) ? ( SQLPOINTER ) HB_UNCONST( hb_parc( 3 ) ) :
                                                 ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                HB_ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) :
                                                 ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
      hb_retni( SQLSetStmtOption( hStmt,
                                  ( SQLUSMALLINT ) hb_parni( 2 ),
                                  HB_ISCHAR( 3 ) ? ( SQLULEN ) ( HB_PTRUINT ) hb_parc( 3 ) :
                                                   ( SQLULEN ) hb_parnl( 3 ) ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETCONNECTATTR )  /* hDbc, nOption, @cOption */
{
   SQLHDBC hDbc = hb_SQLHDBC_par( 1 );

   if( hDbc )
   {
#if ODBCVER >= 0x0300
      SQLPOINTER buffer[ 512 ];
      SQLINTEGER lLen = 0;
      buffer[ 0 ] = NULL;
      hb_retni( SQLGetConnectAttr( hDbc,
                                   ( SQLINTEGER ) hb_parnl( 2 ),
                                   ( SQLPOINTER ) buffer,
                                   ( SQLINTEGER ) sizeof( buffer ),
                                   ( SQLINTEGER * ) &lLen ) );
      hb_storclen( ( char * ) buffer, lLen, 3 );
#else
      char buffer[ 512 ];
      buffer[ 0 ] = '\0';
      hb_retni( SQLGetConnectOption( hDbc,
                                     ( SQLSMALLINT ) hb_parni( 2 ),
                                     ( SQLPOINTER ) buffer ) );
      hb_storc( buffer, 3 );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETSTMTATTR )  /* hStmt, nOption, @cOption */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
#if ODBCVER >= 0x0300
      SQLPOINTER buffer[ 512 ];
      SQLINTEGER lLen = 0;
      buffer[ 0 ] = NULL;
      hb_retni( SQLGetStmtAttr( hStmt,
                                ( SQLINTEGER ) hb_parnl( 2 ),
                                ( SQLPOINTER ) buffer,
                                ( SQLINTEGER ) sizeof( buffer ),
                                ( SQLINTEGER * ) &lLen ) );
      hb_storclen( ( char * ) buffer, lLen, 3 );
#else
      char buffer[ 512 ];
      buffer[ 0 ] = '\0';
      hb_retni( SQLGetStmtOption( hStmt,
                                  ( SQLSMALLINT ) hb_parni( 2 ),
                                  ( SQLPOINTER ) buffer ) );
      hb_storc( buffer, 3 );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLCOMMIT )  /* hEnv, hDbc */
{
   SQLHENV hEnv = hb_SQLHENV_par( 1 );
   SQLHDBC hDbc = hb_SQLHDBC_par( 2 );

   if( hEnv && hDbc )
      hb_retni( SQLTransact( hEnv, hDbc, SQL_COMMIT ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLROLLBACK )  /* hEnv, hDbc */
{
   SQLHENV hEnv = hb_SQLHENV_par( 1 );
   SQLHDBC hDbc = hb_SQLHDBC_par( 2 );

   if( hEnv && hDbc )
      hb_retni( SQLTransact( hEnv, hDbc, SQL_ROLLBACK ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLPREPARE )  /* hStmt, cStatement --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
   {
      void * hStatement;

      hb_retni( SQLPrepare( hStmt,
                            ( SQLTCHAR * ) HB_UNCONST( O_HB_PARSTRDEF( 2, &hStatement, NULL ) ),
                            ( SQLINTEGER ) SQL_NTS ) );

      hb_strfree( hStatement );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLEXECUTE )  /* hStmt --> nRetCode */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
      hb_retni( SQLExecute( hStmt ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLMORERESULTS )  /* hEnv, hDbc */
{
   SQLHSTMT hStmt = hb_SQLHSTMT_par( 1 );

   if( hStmt )
      hb_retni( SQLMoreResults( hStmt ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_ODBCSTOD )
{
   if( hb_parclen( 1 ) >= 10 )
   {
      const char * szSqlDate = hb_parc( 1 );  /* YYYY-MM-DD */
      char         szHrbDate[ 9 ];            /* YYYYMMDD */

      szHrbDate[ 0 ] = szSqlDate[ 0 ];
      szHrbDate[ 1 ] = szSqlDate[ 1 ];
      szHrbDate[ 2 ] = szSqlDate[ 2 ];
      szHrbDate[ 3 ] = szSqlDate[ 3 ];
      szHrbDate[ 4 ] = szSqlDate[ 5 ];
      szHrbDate[ 5 ] = szSqlDate[ 6 ];
      szHrbDate[ 6 ] = szSqlDate[ 8 ];
      szHrbDate[ 7 ] = szSqlDate[ 9 ];
      szHrbDate[ 8 ] = '\0';
      hb_retds( szHrbDate );
   }
   else
      hb_retds( NULL );
}

HB_FUNC( HB_ODBCNUMSETLEN )  /* nValue, nSize, nDecimals --> nValue (nSize, nDec) */
{
   PHB_ITEM pValue = hb_param( 1, HB_IT_NUMERIC );
   int iLen = hb_parni( 2 );
   int iDec = hb_parni( 3 );

   if( pValue != NULL && HB_IS_NUMINT( pValue ) && iDec == 0 )
      hb_retnintlen( hb_itemGetNInt( pValue ), iLen );
   else
      hb_retnlen( hb_itemGetND( pValue ), iLen, iDec );
}

HB_FUNC( HB_ODBCVER )
{
   hb_retni( ODBCVER );
}
