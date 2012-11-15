/*
 * $Id$
 */

/*
 * Harbour Project source code
 * This file contains source for first ODBC routines.
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
 *    SQLNUMRESULTCOLS()
 *    SQLDESCRIBECOL()
 *
 * Copyright 1996 Marcelo Lombardo <lombardo@uol.com.br>
 *    SQLGETINFO()
 *    SQLSETCONNECTOPTION()
 *    SQLSETSTMTOPTION()
 *    SQLGETCONNECTOPTION()
 *    SQLGETSTMTOPTION()
 *    SQLCOMMIT()
 *    SQLROLLBACK()
 *    SQLCOLATTRIBUTE()
 *    SQLBINDOUTPARAM()
 *    SQLMORERESULTS()
 *
 * See COPYING for licensing terms.
 */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapistr.h"
#include "hbset.h"

/* Make sure to not deprecate manual MM functions if
   automatic MM is not enabled yet */
#if ! defined( _HBODBC_AUTO_MM_ ) && ! defined( HB_LEGACY_LEVEL4 )
#  define HB_LEGACY_LEVEL4
#endif

/* Required by headers on Windows */
#if defined( HB_OS_WIN )

/* NOTE: Workaround for OpenWatcom's (tested with 1.9) odbc32.lib implib
         missing the entry for wide version of one function,
         so we turn off UNICODE, until its fixed in OpenWatcom:
            Error! E2028: _SQLSetStmtAttrW@16 is an undefined reference
         [vszakats] */
#  if defined( __WATCOMC__ ) && defined( UNICODE )
#     undef UNICODE
#  endif

#  include <windows.h>

#  if ! defined( WIN32 )
#     define WIN32  /* Required for WIN32_LEAN_AND_MEAN mode */
#  endif
#endif

#include <sql.h>
#include <sqlext.h>

#if ! defined( HB_OS_WIN )
#  if ! defined( SQLLEN ) && ! defined( SQLTCHAR )
typedef unsigned char SQLTCHAR;
#  endif
#endif

#ifndef SQL_NO_DATA
#  define SQL_NO_DATA  SQL_NO_DATA_FOUND
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

static HB_GARBAGE_FUNC( hb_SQLHENV_Destructor )
{
   /* Retrieve image pointer holder */
   SQLHENV * ptr = ( SQLHENV * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ptr )
   {
#if defined( _HBODBC_AUTO_MM_ )
#if ODBCVER >= 0x0300
      SQLFreeHandle( SQL_HANDLE_ENV, ( SQLHANDLE ) *ptr );
#else
      SQLFreeEnv( *ptr );
#endif
#endif

      /* set pointer to NULL to avoid multiple freeing */
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcSQLHENVFuncs =
{
   hb_SQLHENV_Destructor,
   hb_gcDummyMark
};

static SQLHENV hb_SQLHENV_par( int iParam )
{
   SQLHENV * ptr = ( SQLHENV * ) hb_parptrGC( &s_gcSQLHENVFuncs, iParam );

   return ptr ? *ptr : NULL;
}

static SQLHENV hb_SQLHENV_is( int iParam )
{
   return hb_parptrGC( &s_gcSQLHENVFuncs, iParam );
}

static void hb_SQLHENV_stor( SQLHENV hEnv, int iParam )
{
   SQLHENV * ptr = ( SQLHENV * ) hb_gcAllocate( sizeof( SQLHENV ), &s_gcSQLHENVFuncs );

   *ptr = hEnv;

   hb_storptrGC( ( void * ) ptr, iParam );
}

/* GC - SQLHDBC */

static HB_GARBAGE_FUNC( hb_SQLHDBC_Destructor )
{
   /* Retrieve image pointer holder */
   SQLHDBC * ptr = ( SQLHDBC * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ptr )
   {
#if defined( _HBODBC_AUTO_MM_ )
#if ODBCVER >= 0x0300
      SQLFreeHandle( SQL_HANDLE_DBC, ( SQLHANDLE ) *ptr );
#else
      SQLFreeConnect( *ptr );
#endif
#endif

      /* set pointer to NULL to avoid multiple freeing */
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcSQLHDBCFuncs =
{
   hb_SQLHDBC_Destructor,
   hb_gcDummyMark
};

static SQLHDBC hb_SQLHDBC_par( int iParam )
{
   SQLHDBC * ptr = ( SQLHDBC * ) hb_parptrGC( &s_gcSQLHDBCFuncs, iParam );

   return ptr ? *ptr : NULL;
}

static SQLHDBC hb_SQLHDBC_is( int iParam )
{
   return hb_parptrGC( &s_gcSQLHDBCFuncs, iParam );
}

static void hb_SQLHDBC_stor( SQLHDBC hDbc, int iParam )
{
   SQLHDBC * ptr = ( SQLHDBC * ) hb_gcAllocate( sizeof( SQLHDBC ), &s_gcSQLHDBCFuncs );

   *ptr = hDbc;

   hb_storptrGC( ( void * ) ptr, iParam );
}

/* GC - SQLHSTMT */

static HB_GARBAGE_FUNC( hb_SQLHSTMT_Destructor )
{
   /* Retrieve image pointer holder */
   SQLHSTMT * ptr = ( SQLHSTMT * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ptr )
   {
#if defined( _HBODBC_AUTO_MM_ )
#if ODBCVER >= 0x0300
      SQLFreeHandle( SQL_HANDLE_STMT, ( SQLHANDLE ) *ptr );
#else
      SQLFreeStmt( *ptr, SQL_DROP );
#endif
#endif

      /* set pointer to NULL to avoid multiple freeing */
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcSQLHSTMTFuncs =
{
   hb_SQLHSTMT_Destructor,
   hb_gcDummyMark
};

static SQLHSTMT hb_SQLHSTMT_par( int iParam )
{
   SQLHSTMT * ptr = ( SQLHSTMT * ) hb_parptrGC( &s_gcSQLHSTMTFuncs, iParam );

   return ptr ? *ptr : NULL;
}

static SQLHSTMT hb_SQLHSTMT_is( int iParam )
{
   return hb_parptrGC( &s_gcSQLHSTMTFuncs, iParam );
}

static void hb_SQLHSTMT_stor( SQLHSTMT hStmt, int iParam )
{
   SQLHSTMT * ptr = ( SQLHSTMT * ) hb_gcAllocate( sizeof( SQLHSTMT ), &s_gcSQLHSTMTFuncs );

   *ptr = hStmt;

   hb_storptrGC( ( void * ) ptr, iParam );
}

HB_FUNC( SQLALLOCENV ) /* @hEnv --> nRetCode */
{
   SQLHENV hEnv;

#if ODBCVER >= 0x0300
   hb_retni( SQLAllocHandle( SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv ) );
#else
   hb_retni( SQLAllocEnv( &hEnv ) );
#endif

   hb_SQLHENV_stor( hEnv, 1 );
}

HB_FUNC( SQLALLOCCONNECT ) /* hEnv, @hDbc --> nRetCode */
{
   if( hb_SQLHENV_is( 1 ) )
   {
      SQLHENV hEnv = hb_SQLHENV_par( 1 );
      SQLHDBC hDbc;

#if ODBCVER >= 0x0300
      hb_retni( SQLAllocHandle( SQL_HANDLE_DBC, hEnv, &hDbc ) );
#else
      hb_retni( SQLAllocConnect( hEnv, &hDbc ) );
#endif

      hb_SQLHDBC_stor( hDbc, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLDRIVERCONNECT ) /* hDbc, @cConnectString --> nRetCode */
{
   if( hb_SQLHDBC_is( 1 ) )
   {
      SQLSMALLINT iLen;
      SQLRETURN   ret;
      void *      hConnStr;
      SQLTCHAR    buffer[ 1024 ];

      buffer[ 0 ] = '\0';

      ret = SQLDriverConnect( hb_SQLHDBC_par( 1 ),
                              ( SQLHWND ) NULL,
                              ( SQLTCHAR * ) O_HB_PARSTRDEF( 2, &hConnStr, NULL ),
                              ( SQLSMALLINT ) hb_parclen( 2 ),
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

HB_FUNC( SQLCONNECT ) /* hDbc, cDSN, cUseName, cPassword --> nRetCode */
{
   if( hb_SQLHDBC_is( 1 ) )
   {
      SQLRETURN ret;

      void * hDSN;
      void * hUser;
      void * hPass;

      ret = SQLConnect( hb_SQLHDBC_par( 1 ),
                        ( SQLTCHAR * ) O_HB_PARSTRDEF( 2, &hDSN, NULL ),
                        ( SQLSMALLINT ) hb_parclen( 2 ),
                        ( SQLTCHAR * ) O_HB_PARSTRDEF( 3, &hUser, NULL ),
                        ( SQLSMALLINT ) hb_parclen( 3 ),
                        ( SQLTCHAR * ) O_HB_PARSTRDEF( 4, &hPass, NULL ),
                        ( SQLSMALLINT ) hb_parclen( 4 ) );

      hb_strfree( hDSN );
      hb_strfree( hUser );
      hb_strfree( hPass );

      hb_retni( ret );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLDISCONNECT ) /* hDbc --> nRetCode */
{
   if( hb_SQLHDBC_is( 1 ) )
      hb_retni( SQLDisconnect( hb_SQLHDBC_par( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if defined( HB_LEGACY_LEVEL4 )

HB_FUNC( SQLFREECONNECT ) /* hDbc --> nRetCode */
{
#if ! defined( _HBODBC_AUTO_MM_ )
   hb_retni( SQLFreeConnect( hb_SQLHDBC_par( 1 ) ) );
#endif
}

HB_FUNC( SQLFREEENV ) /* hEnv --> nRetCode */
{
#if ! defined( _HBODBC_AUTO_MM_ )
   hb_retni( SQLFreeEnv( hb_SQLHENV_par( 1 ) ) );
#endif
}

HB_FUNC( SQLFREESTMT ) /* hStmt, nType --> nRetCode */
{
#if ! defined( _HBODBC_AUTO_MM_ )
   hb_retni( SQLFreeStmt( hb_SQLHSTMT_par( 1 ),
                          ( SQLUSMALLINT ) hb_parni( 2 ) ) );
#endif
}

#endif

HB_FUNC( SQLALLOCSTMT ) /* hDbc, @hStmt --> nRetCode */
{
   if( hb_SQLHDBC_is( 1 ) )
   {
      SQLHDBC hDbc = hb_SQLHDBC_par( 1 );
      SQLHSTMT hStmt;

#if ODBCVER >= 0x0300
      hb_retni( SQLAllocHandle( SQL_HANDLE_STMT, hDbc, &hStmt ) );
#else
      hb_retni( SQLAllocStmt( hDbc, &hStmt ) );
#endif

      hb_SQLHSTMT_stor( hStmt, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLEXECDIRECT ) /* hStmt, cStatement --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
      void * hStatement;

      hb_retni( SQLExecDirect( hb_SQLHSTMT_par( 1 ),
                               ( SQLTCHAR * ) O_HB_PARSTRDEF( 2, &hStatement, NULL ),
                               ( SQLINTEGER ) hb_parclen( 2 ) ) );
      hb_strfree( hStatement );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLFETCH ) /* hStmt --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
      hb_retni( SQLFetch( hb_SQLHSTMT_par( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETDATA ) /* hStmt, nField, nType, nLen, @cBuffer --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
      SQLLEN      nLen;
      SQLLEN      nInitBuff;
      SQLLEN      nBuffLen = 0;
      char *      buffer;
      char *      outbuf    = NULL;
      SQLSMALLINT iType     = ( SQLSMALLINT ) hb_parnidef( 3, SQL_BINARY );
      int         iReallocs = 0;
      SQLRETURN   result;

      nLen = ( SQLLEN ) hb_parnint( 4 );
      if( nLen <= 0 )
         nLen = 64;
      nInitBuff = nLen;
      buffer    = ( char * ) hb_xgrab( ( HB_SIZE ) nLen + 1 );

      result = ! SQL_NO_DATA;
      while( result != SQL_NO_DATA )
      {
         result = SQLGetData( hb_SQLHSTMT_par( 1 ),
                              ( SQLUSMALLINT ) hb_parni( 2 ),
                              ( SQLSMALLINT ) iType,
                              ( SQLPOINTER ) buffer,
                              ( SQLLEN ) nLen,
                              ( SQLLEN * ) &nLen );

         if( result == SQL_SUCCESS && iReallocs == 0 )
         {
            hb_storclen( buffer, ( HB_SIZE ) ( nLen < 0 ? 0 : ( nLen < ( SQLLEN ) hb_parnint( 4 ) ? nLen : ( SQLLEN ) hb_parnint( 4 ) ) ), 5 );
            break;
         }
         else if( result == SQL_SUCCESS_WITH_INFO && iReallocs == 0 )
         {
            /* Perhaps a data truncation */
            if( nLen >= nInitBuff )
            {
               /* data right truncated! */
               nBuffLen = nLen;
               outbuf   = ( char * ) hb_xgrab( ( HB_SIZE ) nBuffLen + 1 );
               hb_strncpy( outbuf, buffer, nLen );
               nLen   = nLen - nInitBuff + 2;
               buffer = ( char * ) hb_xrealloc( buffer, ( HB_SIZE ) nLen );
               iReallocs++;
            }
            else
            {
               hb_storclen( buffer, ( HB_SIZE ) ( nLen < 0 ? 0 : ( nLen < ( SQLLEN ) hb_parnint( 4 ) ? nLen : ( SQLLEN ) hb_parnint( 4 ) ) ), 5 );
               break;
            }
         }
         else if( ( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO ) && iReallocs > 0 )
         {
            hb_strncat( outbuf, buffer, nBuffLen );
            hb_storclen( outbuf, ( HB_SIZE ) ( nLen + nInitBuff - 1 ), 5 );
            result = SQL_SUCCESS;
            break;
         }
         else
            break;
      }
      hb_xfree( buffer );
      if( outbuf )
         hb_xfree( outbuf );
      hb_retni( result );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLNUMRESULTCOLS ) /* hStmt, @nColCount --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
      SQLSMALLINT iCols = 0;

      hb_retni( SQLNumResultCols( hb_SQLHSTMT_par( 1 ), &iCols ) );

      hb_stornl( ( long ) iCols, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLDESCRIBECOL ) /* hStmt, nCol, @cName, nLen, @nBufferLen, @nDataType, @nColSize, @nDec, @nNull --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
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

      hb_retni( SQLDescribeCol( hb_SQLHSTMT_par( 1 ),
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

HB_FUNC( SQLCOLATTRIBUTE ) /* hStmt, nCol, nField, @cName, nLen, @nBufferLen, @nAttribute --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
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
      hb_retni( SQLColAttribute( hb_SQLHSTMT_par( 1 ),
                                 ( SQLUSMALLINT ) hb_parni( 2 ),
                                 ( SQLUSMALLINT ) hb_parni( 3 ),
                                 ( SQLPOINTER ) buffer,
                                 iLen,
                                 ( SQLSMALLINT * ) &iBufLen,
                                 ( SQLLEN * ) &nNumPtr ) );
#else
      hb_retni( SQLColAttributes( hb_SQLHSTMT_par( 1 ),
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

HB_FUNC( SQLFETCHSCROLL )
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
#if ODBCVER >= 0x0300
      hb_retni( SQLFetchScroll( hb_SQLHSTMT_par( 1 ),
                                ( SQLSMALLINT ) hb_parni( 2 ),
                                ( SQLLEN ) hb_parnint( 3 ) ) );
#else
      hb_retni( SQL_ERROR );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLERROR ) /* hEnv, hDbc, hStmt, @cErrorClass, @nType, @cErrorMsg */
{
   if( hb_SQLHENV_is( 1 ) || hb_SQLHDBC_is( 2 ) || hb_SQLHSTMT_is( 3 ) )
   {
      SQLINTEGER  lError;
      SQLSMALLINT iLen;
      SQLTCHAR    buffer[ 256 ];
      SQLTCHAR    szErrorMsg[ SQL_MAX_MESSAGE_LENGTH + 1 ];

      buffer[ 0 ]     = '\0';
      szErrorMsg[ 0 ] = '\0';
      iLen = 0;

      hb_retni( SQLError( hb_SQLHENV_par( 1 ),
                          hb_SQLHDBC_par( 2 ),
                          hb_SQLHSTMT_par( 3 ),
                          ( SQLTCHAR * ) buffer,
                          ( SQLINTEGER * ) &lError,
                          ( SQLTCHAR * ) szErrorMsg,
                          ( SQLSMALLINT ) sizeof( szErrorMsg ),
                          ( SQLSMALLINT * ) &iLen ) );

      O_HB_STORSTR( ( O_HB_CHAR * ) buffer, 4 );
      hb_stornl( ( long ) lError, 5 );
      O_HB_STORSTRLEN( ( O_HB_CHAR * ) szErrorMsg, iLen, 6 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETDIAGREC ) /* nHandleType, hHandle, nRecNumber, @cSQLState, @nError, @cErrorMsg */
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
                               ( SQLSMALLINT ) sizeof( szErrorMsg ),
                               ( SQLSMALLINT * ) &iLen ) );

      O_HB_STORSTR( ( O_HB_CHAR * ) szSQLState, 4 );
      hb_stornl( ( long ) lError, 5 );
      O_HB_STORSTRLEN( ( O_HB_CHAR * ) szErrorMsg, iLen, 6 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retni( SQL_ERROR );
   hb_storc( "", 4 );
   hb_stornl( 0, 5 );
   hb_storc( "", 6 );
#endif
}

HB_FUNC( SQLROWCOUNT )
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
      SQLLEN iRowCountPtr = ( SQLLEN ) hb_parnint( 2 );

      hb_retni( SQLRowCount( hb_SQLHSTMT_par( 1 ),
                             ( SQLLEN * ) &iRowCountPtr ) );

      hb_stornint( iRowCountPtr, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETINFO ) /* hDbc, nType, @cResult */
{
   if( hb_SQLHDBC_is( 1 ) )
   {
      char        buffer[ 512 ];
      SQLSMALLINT iLen = 0;

      buffer[ 0 ] = '\0';

      hb_retni( SQLGetInfo( hb_SQLHDBC_par( 1 ),
                            ( SQLUSMALLINT ) hb_parni( 2 ),
                            ( SQLPOINTER ) buffer,
                            ( SQLSMALLINT ) sizeof( buffer ),
                            ( SQLSMALLINT * ) &iLen ) );

      hb_storclen( buffer, iLen, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLSETCONNECTATTR ) /* hDbc, nOption, uOption */
{
   if( hb_SQLHDBC_is( 1 ) )
   {
#if ODBCVER >= 0x0300
      hb_retni( SQLSetConnectAttr( hb_SQLHDBC_par( 1 ),
                                   ( SQLINTEGER ) hb_parnl( 2 ),
                                   HB_ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                   HB_ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
      hb_retni( SQLSetConnectOption( hb_SQLHDBC_par( 1 ),
                                     ( SQLUSMALLINT ) hb_parni( 2 ),
                                     HB_ISCHAR( 3 ) ? ( SQLULEN ) hb_parc( 3 ) : ( SQLULEN ) hb_parnl( 3 ) ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLSETSTMTATTR ) /* hStmt, nOption, uOption --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
#if ODBCVER >= 0x0300
      hb_retni( SQLSetStmtAttr( hb_SQLHSTMT_par( 1 ),
                                ( SQLINTEGER ) hb_parnl( 2 ),
                                HB_ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                HB_ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
      hb_retni( SQLSetStmtOption( hb_SQLHSTMT_par( 1 ),
                                  ( SQLUSMALLINT ) hb_parni( 2 ),
                                  HB_ISCHAR( 3 ) ? ( SQLULEN ) hb_parc( 3 ) : ( SQLULEN ) hb_parnl( 3 ) ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETCONNECTATTR ) /* hDbc, nOption, @cOption */
{
   if( hb_SQLHDBC_is( 1 ) )
   {
#if ODBCVER >= 0x0300
      SQLPOINTER buffer[ 512 ];
      SQLINTEGER lLen = 0;
      buffer[ 0 ] = '\0';
      hb_retni( SQLGetConnectAttr( hb_SQLHDBC_par( 1 ),
                                   ( SQLINTEGER ) hb_parnl( 2 ),
                                   ( SQLPOINTER ) buffer,
                                   ( SQLINTEGER ) sizeof( buffer ),
                                   ( SQLINTEGER * ) &lLen ) );
      hb_storclen( ( char * ) buffer, lLen, 3 );
#else
      char buffer[ 512 ];
      buffer[ 0 ] = '\0';
      hb_retni( SQLGetConnectOption( hb_SQLHDBC_par( 1 ),
                                     ( SQLSMALLINT ) hb_parni( 2 ),
                                     ( SQLPOINTER ) buffer ) );
      hb_storc( buffer, 3 );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLGETSTMTATTR ) /* hStmt, nOption, @cOption */
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
#if ODBCVER >= 0x0300
      SQLPOINTER buffer[ 512 ];
      SQLINTEGER lLen = 0;
      buffer[ 0 ] = '\0';
      hb_retni( SQLGetStmtAttr( hb_SQLHSTMT_par( 1 ),
                                ( SQLINTEGER ) hb_parnl( 2 ),
                                ( SQLPOINTER ) buffer,
                                ( SQLINTEGER ) sizeof( buffer ),
                                ( SQLINTEGER * ) &lLen ) );
      hb_storclen( ( char * ) buffer, lLen, 3 );
#else
      char buffer[ 512 ];
      buffer[ 0 ] = '\0';
      hb_retni( SQLGetStmtOption( hb_SQLHSTMT_par( 1 ),
                                  ( SQLSMALLINT ) hb_parni( 2 ),
                                  ( SQLPOINTER ) buffer ) );
      hb_storc( buffer, 3 );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLCOMMIT ) /* hEnv, hDbc */
{
   if( hb_SQLHENV_is( 1 ) && hb_SQLHDBC_is( 2 ) )
      hb_retni( SQLTransact( hb_SQLHENV_par( 1 ), hb_SQLHDBC_par( 2 ), SQL_COMMIT ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLROLLBACK ) /* hEnv, hDbc */
{
   if( hb_SQLHENV_is( 1 ) && hb_SQLHDBC_is( 2 ) )
      hb_retni( SQLTransact( hb_SQLHENV_par( 1 ), hb_SQLHDBC_par( 2 ), SQL_ROLLBACK ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLPREPARE ) /* hStmt, cStatement --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
      void * hStatement;

      hb_retni( SQLPrepare( hb_SQLHSTMT_par( 1 ),
                            ( SQLTCHAR * ) O_HB_PARSTRDEF( 2, &hStatement, NULL ),
                            ( SQLINTEGER ) SQL_NTS ) );

      hb_strfree( hStatement );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLEXECUTE ) /* hStmt --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
      hb_retni( SQLExecute( hb_SQLHSTMT_par( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLMORERESULTS ) /* hEnv, hDbc */
{
   if( hb_SQLHSTMT_is( 1 ) )
      hb_retni( SQLMoreResults( hb_SQLHSTMT_par( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQLBINDPARAMETER ) /* nStatementHandle, nParameterNumber, nParameterType, ColumnSize, DecimalDigits, @ParamValue, @ParamLength --> nRetCode */
{
   if( hb_SQLHSTMT_is( 1 ) )
   {
      SQLLEN nLen = ( SQLLEN ) hb_parnint( 7 );

      hb_retni( SQLBindParameter( hb_SQLHSTMT_par( 1 ),
                                  ( SQLUSMALLINT ) hb_parni( 2 ),
                                  ( SQLSMALLINT ) SQL_PARAM_OUTPUT,
                                  ( SQLSMALLINT ) SQL_CHAR,
                                  ( SQLSMALLINT ) hb_parni( 3 ),
                                  ( SQLULEN ) hb_parnint( 4 ),
                                  ( SQLSMALLINT ) hb_parni( 5 ),
                                  ( SQLPOINTER ) hb_parcx( 6 ),
                                  ( SQLLEN ) hb_parclen( 6 ),
                                  ( SQLLEN * ) &nLen ) );
      hb_stornint( nLen, 7 );
   }
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

HB_FUNC( HB_ODBCNUMSETLEN ) /* nValue, nSize, nDecimals --> nValue (nSize, nDec) */
{
   hb_retnlen( hb_parnd( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

HB_FUNC( HB_ODBCVER )
{
   hb_retni( ODBCVER );
}
