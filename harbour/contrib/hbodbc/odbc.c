/*
 * $Id$
 */

/*
 * Harbour Project source code
 * This file contains source for first ODBC routines.
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
 *    SQLNUMRES()
 *    SQLDESCRIB()
 *    SQLEXTENDE()
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

#define HB_OS_WIN_USED

#include "hbapi.h"

#if defined(HB_OS_LINUX) && defined(__WATCOMC__)
#include "/usr/include/sql.h"
#include "/usr/include/sqlext.h"
#include "/usr/include/sqltypes.h"
#else
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
#endif

#ifndef SQLLEN
   #ifdef HB_OS_WIN_64
      typedef INT64           SQLLEN;
   #else
      #define SQLLEN          SQLINTEGER
   #endif
#endif
#ifndef SQLULEN
   #ifdef HB_OS_WIN_64
      typedef UINT64          SQLULEN;
   #else
      #define SQLULEN         SQLUINTEGER
   #endif
#endif

#ifndef SQL_NO_DATA
   #define SQL_NO_DATA SQL_NO_DATA_FOUND
#endif

HB_FUNC( SQLALLOCENV ) /* SQLALLOCENV( @hEnv ) --> nRetCode */
{
   SQLHENV hEnv;
   SQLRETURN ret = SQLAllocEnv( &hEnv );

   hb_storptr( ( void * ) hEnv, 1 );
   hb_retni( ret );
}

HB_FUNC( SQLALLOCCONNECT ) /* SQLALLOCCONNECT( hEnv, @ hDbc ) --> nRetCode */
{
   SQLHDBC hDbc;
   SQLRETURN ret = SQLAllocConnect( ( SQLHENV ) hb_parptr( 1 ), &hDbc );

   hb_storptr( ( void * ) hDbc, 2 );
   hb_retni( ret );
}

HB_FUNC( SQLDRIVERCONNECT ) /* SQLDRIVERCONNECT( hDbc, @ cConnectString ) --> nRetCode */
{
   SWORD  wLen;
   SQLRETURN ret;

#if defined( HB_OS_WIN ) && defined( UNICODE )

   LPTSTR lpStr = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   TCHAR buffer[ 1024 ];
   buffer[ 0 ] = '\0';
   ret = SQLDriverConnect( ( SQLHDBC ) hb_parptr( 1 ),
                           ( SQLHWND ) GetDesktopWindow(),
                           ( SQLWCHAR * ) lpStr,
                           ( SQLSMALLINT ) hb_parclen( 2 ),
                           ( SQLWCHAR * ) buffer,
                           ( SQLSMALLINT ) sizeof( buffer ),
                           ( SQLSMALLINT * ) &wLen,
                           ( SQLUSMALLINT ) SQL_DRIVER_COMPLETE );
   HB_TCHAR_FREE( lpStr );
   if( ISBYREF( 3 ) )
   {
      char * szStr = HB_TCHAR_CONVFROM( buffer );
      hb_storc( szStr, 3 );
      HB_TCHAR_FREE( szStr );
   }
#else
   BYTE buffer[ 1024 ];
   buffer[ 0 ] = '\0';
   ret = SQLDriverConnect( ( SQLHDBC ) hb_parptr( 1 ),
                           ( SQLHWND ) 0,
                           ( SQLCHAR * ) hb_parcx( 2 ),
                           ( SQLSMALLINT ) hb_parclen( 2 ),
                           ( SQLCHAR * ) buffer,
                           ( SQLSMALLINT ) sizeof( buffer ),
                           ( SQLSMALLINT * ) &wLen,
                           ( SQLUSMALLINT ) SQL_DRIVER_COMPLETE );
   hb_storc( ( char * ) buffer, 3 );
#endif
   hb_retni( ret );
}

HB_FUNC( SQLCONNECT ) /* SQLCONNECT( hDbc, cDSN, cUseName, cPassword ) --> nRetCode */
{
   SQLRETURN ret;
#if defined( HB_OS_WIN ) && defined( UNICODE )
   LPTSTR lpDSN      = HB_TCHAR_CONVTO( hb_parcx( 2 ) ),
          lpUseName  = HB_TCHAR_CONVTO( hb_parcx( 3 ) ),
          lpPassword = HB_TCHAR_CONVTO( hb_parcx( 4 ) );

   ret =  SQLConnect( ( SQLHDBC ) hb_parptr( 1 ),
                      ( SQLWCHAR * ) lpDSN,
                      ( SQLSMALLINT ) hb_parclen( 2 ),
                      ( SQLWCHAR * ) lpUseName,
                      ( SQLSMALLINT ) hb_parclen( 3 ),
                      ( SQLWCHAR * ) lpPassword,
                      ( SQLSMALLINT ) hb_parclen( 4 ) );

   HB_TCHAR_FREE( lpDSN );
   HB_TCHAR_FREE( lpUseName );
   HB_TCHAR_FREE( lpPassword );
#else
   ret =  SQLConnect( ( SQLHDBC ) hb_parptr( 1 ),
                      ( SQLCHAR * ) hb_parcx( 2 ),
                      ( SQLSMALLINT ) hb_parclen( 2 ),
                      ( SQLCHAR * ) hb_parcx( 3 ),
                      ( SQLSMALLINT ) hb_parclen( 3 ),
                      ( SQLCHAR * ) hb_parcx( 4 ),
                      ( SQLSMALLINT ) hb_parclen( 4 ) );
#endif
   hb_retni( ret );
}

HB_FUNC( SQLDISCONNECT ) /* SQLDISCONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLDisconnect( ( SQLHDBC ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLFREECONNECT ) /* SQLFREECONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLFreeConnect( ( SQLHDBC ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLFREEENV ) /* SQLFREEENV( hEnv ) --> nRetCode */
{
   hb_retni( SQLFreeEnv( ( SQLHENV ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLALLOCSTMT ) /* SQLALLOCSTMT( hDbc, @hStmt ) --> nRetCode */
{
   SQLHSTMT hStmt;

   hb_retni( SQLAllocStmt( ( SQLHDBC ) hb_parptr( 1 ), &hStmt ) );
   hb_storptr( ( void * ) hStmt, 2 );
}

HB_FUNC( SQLFREESTMT ) /* SQLFREESTMT( hStmt, nType ) --> nRetCode */
{
   hb_retni( SQLFreeStmt( ( SQLHSTMT ) hb_parptr( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ) ) );
}

HB_FUNC( SQLEXECDIRECT ) /* SQLEXECDIRECT( hStmt, cStatement ) --> nRetCode */
{
#if defined( HB_OS_WIN ) && defined( UNICODE )
   SQLWCHAR * lpStr = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   hb_retni( SQLExecDirect( ( SQLHSTMT ) hb_parptr( 1 ), ( SQLWCHAR * ) lpStr, ( SQLINTEGER ) hb_parclen( 2 ) ) );
   HB_TCHAR_FREE( lpStr );
#else
   hb_retni( SQLExecDirect( ( SQLHSTMT ) hb_parptr( 1 ), ( SQLCHAR * ) hb_parcx( 2 ), ( SQLINTEGER ) hb_parclen( 2 ) ) );
#endif
}

HB_FUNC( SQLFETCH )   /* SQLFETCH( hStmt ) --> nRetCode */
{
   hb_retni( SQLFetch( ( SQLHSTMT ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLGETDATA ) /* SQLGETDATA( hStmt, nField, nType, nLen, @cBuffer ) --> nRetCode */
{
   SQLLEN lLen, lInitBuff, lBuffLen;
   void * bBuffer;
   void * bOut;
   SQLSMALLINT wType;
   SQLRETURN result;
   int iReallocs = 0;

   wType = ( SQLSMALLINT ) hb_parni( 3 );
   if( ! wType )
      wType = SQL_BINARY;
   lLen = ( SQLLEN ) hb_parnint( 4 );
   if( ! lLen )
      lLen = 64;
   bBuffer    = hb_xgrab( ( ULONG ) lLen + 1 );
   bOut       = NULL;
   lInitBuff  = lLen;
   lBuffLen   = 0;

   result = ! SQL_NO_DATA;
   while( result != SQL_NO_DATA )
   {
      result = SQLGetData( ( SQLHSTMT ) hb_parptr( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ), wType, ( SQLPOINTER ) bBuffer, lLen, &lLen );
      if( result == SQL_SUCCESS && iReallocs == 0 )
      {
         hb_storclen( ( char * ) bBuffer, ( long ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
         break;
      }
      else if( result == SQL_SUCCESS_WITH_INFO && iReallocs == 0 )
      {
         /* Perhaps a data truncation */
         if( lLen >= lInitBuff )
         {
            /* data right truncated! */
            lBuffLen = lLen;
            bOut = ( char * ) hb_xgrab( ( ULONG ) lBuffLen + 1 );
            hb_strncpy( ( char * ) bOut, ( char * ) bBuffer, lLen );
            lLen = lLen - lInitBuff + 2;
            bBuffer = ( char * ) hb_xrealloc( bBuffer, ( ULONG ) lLen );
            iReallocs++;
         }
         else
         {
            hb_storclen( ( char * ) bBuffer, ( long ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
            break;
         }
      }
      else if( ( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO ) && iReallocs > 0 )
      {
         hb_strncat( ( char * ) bOut, ( char * ) bBuffer, lBuffLen );
         hb_storclen( ( char * ) bOut, ( long ) ( lLen + lInitBuff - 1 ), 5 );
         result = SQL_SUCCESS;
         break;
      }
      else
         break;
   }
   hb_xfree( bBuffer );
   if( bOut )
      hb_xfree( bOut );
   hb_retni( result );
}

HB_FUNC( SQLNUMRESULTCOLS ) /* HB_NUMRESULTCOLS( hStmt, @nColCount ) */
{
   SQLSMALLINT nCols;
   SQLRETURN result = SQLNumResultCols( ( SQLHSTMT ) hb_parptr( 1 ), &nCols );

/* if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO ) */
      hb_stornl( ( long ) nCols, 2 );

   hb_retni( result );
}

HB_FUNC( SQLDESCRIBECOL ) /* SQLDESCRIBECOL( hStmt, nCol, @cName, nLen, @nBufferLen, @nDataType, @nColSize, @nDec, @nNull ) --> nRetCode */
{
   SQLSMALLINT lLen      = ( SQLSMALLINT ) hb_parni( 4 );
   SQLSMALLINT wBufLen   = ( SQLUSMALLINT ) hb_parni( 5 );
   SQLSMALLINT wDataType = ( SQLUSMALLINT ) hb_parni( 6 );
   SQLULEN     wColSize  = ( SQLULEN ) hb_parnint( 7 );
   SQLSMALLINT wDecimals = ( SQLUSMALLINT ) hb_parni( 8 );
   SQLSMALLINT wNullable = ( SQLUSMALLINT ) hb_parni( 9 );
#if defined( HB_OS_WIN ) && defined( UNICODE )
   LPTSTR      buffer    = ( LPTSTR ) hb_xgrab( lLen * sizeof( TCHAR ) );
#else
   SQLCHAR *   buffer    = ( SQLCHAR * ) hb_xgrab( lLen * sizeof( SQLCHAR ) );
#endif
   SQLRETURN   result;

   result = SQLDescribeCol( ( SQLHSTMT ) hb_parptr( 1 ),
                            ( SQLUSMALLINT ) hb_parni( 2 ),
                            ( SQLCHAR * ) buffer,
                            ( SQLSMALLINT ) lLen,
                            ( SQLSMALLINT * ) &wBufLen,
                            ( SQLSMALLINT * ) &wDataType,
                            ( SQLULEN * ) &wColSize,
                            ( SQLSMALLINT * ) &wDecimals,
                            ( SQLSMALLINT * ) &wNullable );

   if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
   {
      if( ISBYREF( 3 ) )
      {
#if defined( HB_OS_WIN ) && defined( UNICODE )
         char * szStr = HB_TCHAR_CONVFROM( buffer );
         hb_storc( szStr, 3 );
         HB_TCHAR_FREE( szStr );
#else
         hb_storclen( ( char * ) buffer, ( long ) wBufLen, 3 );
#endif
      }
      hb_storni( ( int ) wBufLen, 5 );
      hb_storni( ( int ) wDataType, 6 );
      hb_stornint( wColSize, 7 );
      hb_storni( ( int ) wDecimals, 8 );
      hb_storni( ( int ) wNullable, 9 );
   }

   hb_xfree( buffer );
   hb_retni( result );
}

HB_FUNC( SQLCOLATTRIBUTE ) /* SQLCOLATTRIBUTE( hStmt, nCol, nField, @cName, nLen, @nBufferLen, @nAttribute ) --> nRetCode */
{
   void *      bBuffer   = hb_xgrab( hb_parnl( 5 ) );
   SQLSMALLINT wBufLen   = ( SQLUSMALLINT ) hb_parni( 6 );
   SQLRETURN   result;
#if ODBCVER >= 0x0300
   SQLLEN      wNumPtr   = ( SQLLEN ) hb_parnint( 7 );
   result = SQLColAttribute( ( SQLHSTMT ) hb_parptr( 1 ),
                             ( SQLUSMALLINT ) hb_parni( 2 ),
                             ( SQLUSMALLINT ) hb_parni( 3 ),
                             ( SQLPOINTER ) bBuffer,
                             ( SQLUSMALLINT ) hb_parni( 5 ),
                             ( SQLSMALLINT * ) &wBufLen,
                             ( SQLLEN * ) &wNumPtr );
#else
   SQLINTEGER  wNumPtr   = hb_parnl( 7 );
   result = SQLColAttributes( ( SQLHSTMT ) hb_parptr( 1 ),
                              ( SQLUSMALLINT ) hb_parni( 2 ),
                              ( SQLUSMALLINT ) hb_parni( 3 ),
                              ( SQLPOINTER ) bBuffer,
                              ( SQLUSMALLINT ) hb_parni( 5 ),
                              ( SQLSMALLINT * ) &wBufLen,
                              ( SQLINTEGER * ) &wNumPtr );
#endif

   if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
   {
      hb_storclen( ( char * ) bBuffer, ( long ) wBufLen, 4 );
      hb_storni( ( int ) wBufLen, 6 );
      hb_stornint( wNumPtr, 7 );
   }

   hb_xfree( bBuffer );
   hb_retni( result );
}

HB_FUNC( SQLEXTENDEDFETCH ) /* SQLEXTENDEDFETCH( hStmt, nOrientation, nOffset, @nRows, @nRowStatus ) */
{
   SQLUSMALLINT  siRowStatus   = ( SQLUSMALLINT ) hb_parni( 5 );
#if defined(__POCC__) || defined(__XCC__)
   SQLROWSETSIZE uiRowCountPtr = ( SQLROWSETSIZE ) hb_parnl( 4 );
   SQLRETURN     result        = SQLExtendedFetch( ( SQLHSTMT ) hb_parptr( 1 ),
                                                   ( SQLUSMALLINT ) hb_parni( 2 ),
                                                   ( SQLROWOFFSET ) hb_parnl( 3 ),
                                                   ( SQLROWSETSIZE * ) &uiRowCountPtr,
                                                   ( SQLUSMALLINT * ) &siRowStatus );
#else
   SQLULEN       uiRowCountPtr = ( SQLULEN ) hb_parnint( 4 );
   SQLRETURN     result        = SQLExtendedFetch( ( SQLHSTMT ) hb_parptr( 1 ),
                                                   ( SQLUSMALLINT ) hb_parni( 2 ),
                                                   ( SQLLEN ) hb_parnint( 3 ),
                                                   ( SQLULEN * ) &uiRowCountPtr,
                                                   ( SQLUSMALLINT * ) &siRowStatus );
#endif

   if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
   {
      hb_stornint( uiRowCountPtr, 4 );
      hb_storni( ( int ) siRowStatus, 5 );
   }

   hb_retni( result );
}

HB_FUNC( SQLFETCHSCROLL )
{
#if ODBCVER >= 0x0300
   hb_retni( SQLFetchScroll( ( SQLHSTMT ) hb_parptr( 1 ),
                             ( SQLSMALLINT ) hb_parnl( 2 ),
                             ( SQLLEN ) hb_parnint( 3 ) ) );
#else
   hb_retni( SQL_ERROR );
#endif
}

HB_FUNC( SQLERROR ) /* hEnv, hDbc, hStmt, @ cErrorClass, @ nType, @ cErrorMsg */
{
   SQLINTEGER lError;
   SWORD      wLen;
#if defined( HB_OS_WIN ) && defined( UNICODE )
   TCHAR      buffer[ 256 ], szErrorMsg[ 256 ];
#else
   BYTE       buffer[ 256 ], szErrorMsg[ 256 ];
#endif
   hb_retni( SQLError( ( SQLHENV ) hb_parptr( 1 ),
                       ( SQLHDBC ) hb_parptr( 2 ),
                       ( SQLHSTMT ) ( HB_PTRUINT ) hb_parnint( 3 ),
                       buffer,
                       &lError,
                       szErrorMsg,
                       sizeof( szErrorMsg ),
                       &wLen ) );

   if( ISBYREF( 4 ) )
   {
#if defined( HB_OS_WIN ) && defined( UNICODE )
      char * szStr = HB_TCHAR_CONVFROM( buffer );
      hb_storc( szStr, 4 );
      HB_TCHAR_FREE( szStr );
#else
      hb_storc( ( char * ) buffer, 4 );
#endif
   }
   hb_stornl( ( LONG ) lError, 5 );
   if( ISBYREF( 6 ) )
   {
#if defined( HB_OS_WIN ) && defined( UNICODE )
      char * szStr = HB_TCHAR_CONVFROM( szErrorMsg );
      hb_storc( szStr, 6 );
      HB_TCHAR_FREE( szStr );
#else
      hb_storc( ( char * ) szErrorMsg, 6 );
#endif
   }
}

HB_FUNC( SQLROWCOUNT )
{
    SQLLEN    iRowCountPtr = ( SQLLEN ) hb_parnint( 2 );
    SQLRETURN result       = SQLRowCount( ( SQLHSTMT ) hb_parptr( 1 ),
                                          ( SQLLEN * ) &iRowCountPtr );

    if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
       hb_stornint( iRowCountPtr, 2 );

    hb_retni( result );
}

HB_FUNC( SQLGETINFO ) /* hDbc, nType, @cResult */
{
   BYTE bBuffer[ 512 ];
   SQLSMALLINT wLen;
   SQLRETURN result = SQLGetInfo( ( SQLHDBC ) hb_parptr( 1 ),
                                  ( SQLUSMALLINT ) hb_parnl( 2 ),
                                  ( SQLPOINTER ) bBuffer,
                                  ( SQLSMALLINT ) sizeof( bBuffer ),
                                  ( SQLSMALLINT * ) &wLen );

   hb_storclen( ( char * ) bBuffer, wLen, 3 );
   hb_retni( result );
}

HB_FUNC( SQLSETCONNECTATTR ) /* hDbc, nOption, uOption */
{
#if ODBCVER >= 0x0300
   hb_retni( SQLSetConnectAttr( ( SQLHDBC ) hb_parptr( 1 ),
                                ( SQLINTEGER ) hb_parnl( 2 ),
                                ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parcx( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
   hb_retni( SQLSetConnectOption( ( SQLHDBC ) hb_parptr( 1 ),
                                  ( UWORD ) hb_parnl( 2 ),
                                  ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parcx( 3 ) : hb_parnl( 3 ) ) );
#endif
}

HB_FUNC( SQLSETSTMTATTR ) /* hStmt, nOption, uOption )  --> nRetCode */
{
#if ODBCVER >= 0x0300
   hb_retni( SQLSetStmtAttr( ( SQLHSTMT ) hb_parptr( 1 ),
                             ( SQLINTEGER ) hb_parnl( 2 ),
                             ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parcx( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                             ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
   hb_retni( SQLSetStmtOption( ( SQLHSTMT ) hb_parptr( 1 ),
                               ( UWORD ) hb_parnl( 2 ),
                               ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parcx( 3 ) : hb_parnl( 3 ) ) );
#endif
}

HB_FUNC( SQLGETCONNECTATTR ) /* hDbc, nOption, @cOption */
{
#if ODBCVER >= 0x0300
   SQLPOINTER buffer[ 512 ];
   SQLINTEGER len;
   SQLRETURN result = SQLGetConnectAttr( ( SQLHDBC ) hb_parptr( 1 ),
                                         ( SQLINTEGER ) hb_parnl( 2 ),
                                         ( SQLPOINTER ) buffer,
                                         ( SQLINTEGER ) sizeof( buffer ),
                                         ( SQLINTEGER * ) &len );
   hb_storclen( result == SQL_SUCCESS ? ( char * ) buffer : NULL, len, 3 );
   hb_retni( result );
#else
   BYTE bBuffer[ 512 ];
   SQLRETURN result = SQLGetConnectOption( ( SQLHDBC ) hb_parptr( 1 ),
                                           ( SQLSMALLINT ) hb_parni( 2 ),
                                           ( SQLPOINTER ) bBuffer );

   hb_storclen( result == SQL_SUCCESS ? ( char * ) bBuffer : NULL, sizeof( bBuffer ), 3 );
   hb_retni( result );
#endif
}

HB_FUNC( SQLGETSTMTATTR ) /* hStmt, nOption, @cOption */
{
#if ODBCVER >= 0x0300
   SQLPOINTER buffer[ 512 ];
   SQLINTEGER len;
   SQLRETURN result = SQLGetStmtAttr( ( SQLHSTMT ) hb_parptr( 1 ),
                                      ( SQLINTEGER ) hb_parnl( 2 ),
                                      ( SQLPOINTER ) buffer,
                                      ( SQLINTEGER ) sizeof( buffer ),
                                      ( SQLINTEGER * ) &len );

   hb_storclen( result == SQL_SUCCESS ? ( char * ) buffer : NULL, len, 3 );
   hb_retni( result );
#else
   BYTE bBuffer[ 512 ];
   SQLRETURN result = SQLGetStmtOption( ( SQLHSTMT ) hb_parptr( 1 ),
                                        ( SQLSMALLINT ) hb_parni( 2 ),
                                        ( SQLPOINTER ) bBuffer );

   hb_storclen( result == SQL_SUCCESS ? ( char * ) bBuffer : NULL, sizeof( bBuffer ), 3 );
   hb_retni( result );
#endif
}

HB_FUNC( SQLCOMMIT ) /* hEnv, hDbc */
{
   hb_retni( SQLTransact( ( SQLHENV ) hb_parptr( 1 ), ( SQLHDBC ) hb_parptr( 2 ), SQL_COMMIT ) );
}

HB_FUNC( SQLROLLBACK )  /* hEnv, hDbc */
{
   hb_retni( SQLTransact( ( SQLHENV ) hb_parptr( 1 ), ( SQLHDBC ) hb_parptr( 2 ), SQL_ROLLBACK ) );
}

HB_FUNC( SQLPREPARE )  /* SQLPREPARE( hStmt, cStatement ) --> nRetCode */
{
#if defined( HB_OS_WIN ) && defined( UNICODE )
   LPTSTR lpStr = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   hb_retni( SQLPrepare( ( SQLHSTMT ) hb_parptr( 1 ), lpStr, SQL_NTS ) );
   HB_TCHAR_FREE( lpStr );
#else
   hb_retni( SQLPrepare( ( SQLHSTMT ) hb_parptr( 1 ), ( unsigned char * ) hb_parcx( 2 ), SQL_NTS ) );
#endif
}

HB_FUNC( SQLEXECUTE )  /* SQLEXECUTE( hStmt ) --> nRetCode */
{
   hb_retni( SQLExecute( ( SQLHSTMT ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLEXECUTESCALAR )
{
   SQLHSTMT hStmt;
   SQLLEN lLen;
   BYTE bBuffer[ 256 ];
   SWORD result;

   result = SQLAllocStmt( ( SQLHDBC ) hb_parptr( 2 ), &hStmt );

   if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
   {
#if defined( HB_OS_WIN ) && defined( UNICODE )
      SQLWCHAR * lpStr = HB_TCHAR_CONVTO( hb_parcx( 1 ) );
      result = SQLExecDirect( ( SQLHSTMT ) hStmt, ( SQLWCHAR * ) lpStr, SQL_NTS );
      HB_TCHAR_FREE( lpStr );
#else
      result = SQLExecDirect( ( SQLHSTMT ) hStmt, ( SQLCHAR * ) hb_parcx( 1 ), SQL_NTS );
#endif
      if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
      {
         result = SQLFetch( ( SQLHSTMT ) hStmt );
         if( result != SQL_NO_DATA )
         {
            result = SQLGetData( ( SQLHSTMT ) hStmt, 1, SQL_C_CHAR, ( SQLPOINTER ) bBuffer, sizeof( bBuffer ), &lLen );
            hb_storc( ( char * ) bBuffer, 3 );
         }
      }
   }

   hb_retni( result );

   SQLFreeStmt( ( SQLHSTMT ) hStmt, 0 );
}

HB_FUNC( SQLMORERESULTS ) /* hEnv, hDbc */
{
   hb_retni( SQLMoreResults( ( SQLHSTMT ) hb_parptr( 1 ) ) );
}

#if 0
HB_FUNC( SQLBINDOUTPARAM ) /* SqlBindOutParam( nStatementHandle, nParameterNumber, nParameterType, ColumnSize, DecimalDigits, @ParamValue, @ParamLength    ) --> nRetCode */
{
   SQLLEN lLen = hb_parnint( 7 );
   SQLRETURN ret;

   ret = SQLBindParameter( ( SQLHSTMT ) hb_parptr( 1 ),
                           ( SQLUSMALLINT ) hb_parni( 2 ),
                           ( SQLSMALLINT ) SQL_PARAM_OUTPUT,
                           ( SQLSMALLINT ) SQL_CHAR,
                           ( SQLSMALLINT ) hb_parni( 3 ),
                           ( SQLULEN ) hb_parnint( 4 ),
                           ( SQLSMALLINT ) hb_parni( 5 ),
                           ( SQLPOINTER ) hb_parcx( 6 ),
                           ( SQLINTEGER ) hb_parclen( 6 ),
                           ( SQLLEN * ) &lLen );
   hb_stornint( lLen, 7 );
   hb_retni( ret );
}
#endif

HB_FUNC( SQLSTOD )
{
   if( hb_parclen( 1 ) >= 10 )
   {
      char * szSqlDate = hb_parc( 1 );  /* YYYY-MM-DD */
      char szHrbDate[ 9 ];              /* YYYYMMDD */

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

HB_FUNC( SETNUMLEN )  /* SETNUMLEN( nValue, nSize, nDecimals ) ==> nValue (nSize, nDec) */
{
   hb_retnlen( hb_parnd( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

#if !defined(HB_ODBC_LEGACY_LEVEL_OFF)

HB_FUNC_EXTERN( SQLALLOCENV            ); HB_FUNC( SQLALLOCEN          ) { HB_FUNC_EXEC( SQLALLOCENV            ); }
HB_FUNC_EXTERN( SQLALLOCCONNECT        ); HB_FUNC( SQLALLOCCO          ) { HB_FUNC_EXEC( SQLALLOCCONNECT        ); }
HB_FUNC_EXTERN( SQLDRIVERCONNECT       ); HB_FUNC( SQLDRIVERC          ) { HB_FUNC_EXEC( SQLDRIVERCONNECT       ); }
HB_FUNC_EXTERN( SQLDISCONNECT          ); HB_FUNC( SQLDISCONN          ) { HB_FUNC_EXEC( SQLDISCONNECT          ); }
HB_FUNC_EXTERN( SQLFREECONNECT         ); HB_FUNC( SQLFREECON          ) { HB_FUNC_EXEC( SQLFREECONNECT         ); }
HB_FUNC_EXTERN( SQLALLOCSTMT           ); HB_FUNC( SQLALLOCST          ) { HB_FUNC_EXEC( SQLALLOCSTMT           ); }
HB_FUNC_EXTERN( SQLFREESTMT            ); HB_FUNC( SQLFREESTM          ) { HB_FUNC_EXEC( SQLFREESTMT            ); }
HB_FUNC_EXTERN( SQLEXECDIRECT          ); HB_FUNC( SQLEXECDIR          ) { HB_FUNC_EXEC( SQLEXECDIRECT          ); }
HB_FUNC_EXTERN( SQLNUMRESULTCOLS       ); HB_FUNC( SQLNUMRES           ) { HB_FUNC_EXEC( SQLNUMRESULTCOLS       ); }
HB_FUNC_EXTERN( SQLDESCRIBECOL         ); HB_FUNC( SQLDESCRIB          ) { HB_FUNC_EXEC( SQLDESCRIBECOL         ); }
HB_FUNC_EXTERN( SQLEXTENDEDFETCH       ); HB_FUNC( SQLEXTENDE          ) { HB_FUNC_EXEC( SQLEXTENDEDFETCH       ); }
HB_FUNC_EXTERN( SQLFETCHSCROLL         ); HB_FUNC( SQLFETCHSC          ) { HB_FUNC_EXEC( SQLFETCHSCROLL         ); }
HB_FUNC_EXTERN( SQLROWCOUNT            ); HB_FUNC( SQLROWCOUN          ) { HB_FUNC_EXEC( SQLROWCOUNT            ); }
HB_FUNC_EXTERN( SQLSETCONNECTATTR      ); HB_FUNC( SQLSETCONNECTOPTION ) { HB_FUNC_EXEC( SQLSETCONNECTATTR      ); }
HB_FUNC_EXTERN( SQLSETSTMTATTR         ); HB_FUNC( SQLSETSTMTOPTION    ) { HB_FUNC_EXEC( SQLSETSTMTATTR         ); }
HB_FUNC_EXTERN( SQLGETCONNECTATTR      ); HB_FUNC( SQLGETCONNECTOPTION ) { HB_FUNC_EXEC( SQLGETCONNECTATTR      ); }
HB_FUNC_EXTERN( SQLGETSTMTATTR         ); HB_FUNC( SQLGETSTMTOPTION    ) { HB_FUNC_EXEC( SQLGETSTMTATTR         ); }

#endif
