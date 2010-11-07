/*
 * $Id$
 */

/*
 * Harbour Project source code
 * This file contains source for first ODBC routines.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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
#include "hbapistr.h"
#include "hbset.h"

/* Required by headers on Windows */
#if defined( HB_OS_WIN )
#  include <windows.h>
   /* Required for WIN32_LEAN_AND_MEAN mode */
#  if ! defined( WIN32 )
#     define WIN32
#  endif
#endif

#include <sql.h>
#include <sqlext.h>

#if !defined( HB_OS_WIN )
#  if !defined( SQLLEN ) && !defined( SQLTCHAR )
      typedef unsigned char   SQLTCHAR;
#  endif
#endif

#ifndef SQL_NO_DATA
#  define SQL_NO_DATA     SQL_NO_DATA_FOUND
#endif

#if defined( UNICODE )
   #define O_HB_PARSTR( n, h, len )                hb_parstr_u16( n, HB_CDP_ENDIAN_NATIVE, h, len )
   #define O_HB_PARSTRDEF( n, h, len )             hb_wstrnull( hb_parstr_u16( n, HB_CDP_ENDIAN_NATIVE, h, len ) )
   #define O_HB_STORSTR( str, n )                  hb_storstr_u16( HB_CDP_ENDIAN_NATIVE, str, n )
   #define O_HB_STORSTRLEN( str, len, n )          hb_storstrlen_u16( HB_CDP_ENDIAN_NATIVE, str, len, n )
   #define O_HB_ARRAYGETSTR( arr, n, phstr, plen ) hb_arrayGetStrU16( arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define O_HB_ITEMCOPYSTR( itm, str, len )       hb_itemCopyStrU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define O_HB_ITEMGETSTR( itm, phstr, plen )     hb_itemGetStrU16( itm, HB_CDP_ENDIAN_NATIVE, phstr, plen )
   #define O_HB_ITEMPUTSTR( itm, str )             hb_itemPutStrU16( itm, HB_CDP_ENDIAN_NATIVE, str )
   #define O_HB_ITEMPUTSTRLEN( itm, str, len )     hb_itemPutStrLenU16( itm, HB_CDP_ENDIAN_NATIVE, str, len )
   #define O_HB_CHAR HB_WCHAR
#else
   #define O_HB_PARSTR( n, h, len )                hb_parstr( n, hb_setGetOSCP(), h, len )
   #define O_HB_PARSTRDEF( n, h, len )             hb_strnull( hb_parstr( n, hb_setGetOSCP(), h, len ) )
   #define O_HB_STORSTR( str, n )                  hb_storstr( hb_setGetOSCP(), str, n )
   #define O_HB_STORSTRLEN( str, len, n )          hb_storstrlen( hb_setGetOSCP(), str, len, n )
   #define O_HB_ARRAYGETSTR( arr, n, phstr, plen ) hb_arrayGetStr( arr, n, hb_setGetOSCP(), phstr, plen )
   #define O_HB_ITEMCOPYSTR( itm, str, len )       hb_itemCopyStr( itm, hb_setGetOSCP(), str, len )
   #define O_HB_ITEMGETSTR( itm, phstr, plen )     hb_itemGetStr( itm, hb_setGetOSCP(), phstr, plen )
   #define O_HB_ITEMPUTSTR( itm, str )             hb_itemPutStr( itm, hb_setGetOSCP(), str )
   #define O_HB_ITEMPUTSTRLEN( itm, str, len )     hb_itemPutStrLen( itm, hb_setGetOSCP(), str, len )
   #define O_HB_CHAR char
#endif

HB_FUNC( SQLALLOCENV ) /* @hEnv --> nRetCode */
{
   SQLHENV hEnv;
   hb_retni( SQLAllocEnv( &hEnv ) );
   hb_storptr( ( void * ) hEnv, 1 );
}

HB_FUNC( SQLALLOCCONNECT ) /* hEnv, @hDbc --> nRetCode */
{
   SQLHDBC hDbc;
   hb_retni( SQLAllocConnect( ( SQLHENV ) hb_parptr( 1 ), &hDbc ) );
   hb_storptr( ( void * ) hDbc, 2 );
}

HB_FUNC( SQLDRIVERCONNECT ) /* hDbc, @cConnectString --> nRetCode */
{
   SQLSMALLINT wLen;
   SQLRETURN ret;
   void * hConnStr;
   SQLTCHAR buffer[ 1024 ];

   buffer[ 0 ] = '\0';

   ret = SQLDriverConnect( ( SQLHDBC ) hb_parptr( 1 ),
                           ( SQLHWND ) NULL,
                           ( SQLTCHAR * ) O_HB_PARSTRDEF( 2, &hConnStr, NULL ),
                           ( SQLSMALLINT ) hb_parclen( 2 ),
                           ( SQLTCHAR * ) buffer,
                           ( SQLSMALLINT ) HB_SIZEOFARRAY( buffer ),
                           ( SQLSMALLINT * ) &wLen,
                           ( SQLUSMALLINT ) SQL_DRIVER_COMPLETE );

   hb_strfree( hConnStr );

   O_HB_STORSTR( ( O_HB_CHAR * ) buffer, 3 );

   hb_retni( ret );
}

HB_FUNC( SQLCONNECT ) /* hDbc, cDSN, cUseName, cPassword --> nRetCode */
{
   SQLRETURN ret;

   void * hDSN;
   void * hUser;
   void * hPass;

   ret =  SQLConnect( ( SQLHDBC ) hb_parptr( 1 ),
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

HB_FUNC( SQLDISCONNECT ) /* hDbc --> nRetCode */
{
   hb_retni( SQLDisconnect( ( SQLHDBC ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLFREECONNECT ) /* hDbc --> nRetCode */
{
   hb_retni( SQLFreeConnect( ( SQLHDBC ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLFREEENV ) /* hEnv --> nRetCode */
{
   hb_retni( SQLFreeEnv( ( SQLHENV ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLALLOCSTMT ) /* hDbc, @hStmt --> nRetCode */
{
   SQLHSTMT hStmt;
   hb_retni( SQLAllocStmt( ( SQLHDBC ) hb_parptr( 1 ), &hStmt ) );
   hb_storptr( ( void * ) hStmt, 2 );
}

HB_FUNC( SQLFREESTMT ) /* hStmt, nType --> nRetCode */
{
   hb_retni( SQLFreeStmt( ( SQLHSTMT ) hb_parptr( 1 ),
                          ( SQLUSMALLINT ) hb_parni( 2 ) ) );
}

HB_FUNC( SQLEXECDIRECT ) /* hStmt, cStatement --> nRetCode */
{
   void * hStatement;
   hb_retni( SQLExecDirect( ( SQLHSTMT ) hb_parptr( 1 ),
                            ( SQLTCHAR * ) O_HB_PARSTRDEF( 2, &hStatement, NULL ),
                            ( SQLINTEGER ) hb_parclen( 2 ) ) );
   hb_strfree( hStatement );
}

HB_FUNC( SQLFETCH ) /* hStmt --> nRetCode */
{
   hb_retni( SQLFetch( ( SQLHSTMT ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLGETDATA ) /* hStmt, nField, nType, nLen, @cBuffer --> nRetCode */
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
   bBuffer    = hb_xgrab( ( HB_SIZE ) lLen + 1 );
   bOut       = NULL;
   lInitBuff  = lLen;
   lBuffLen   = 0;

   result = ! SQL_NO_DATA;
   while( result != SQL_NO_DATA )
   {
      result = SQLGetData( ( SQLHSTMT ) hb_parptr( 1 ),
                           ( SQLUSMALLINT ) hb_parni( 2 ),
                           ( SQLSMALLINT ) wType,
                           ( SQLPOINTER ) bBuffer,
                           ( SQLLEN ) lLen,
                           ( SQLLEN * ) &lLen );

      if( result == SQL_SUCCESS && iReallocs == 0 )
      {
         hb_storclen( ( char * ) bBuffer, ( HB_SIZE ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
         break;
      }
      else if( result == SQL_SUCCESS_WITH_INFO && iReallocs == 0 )
      {
         /* Perhaps a data truncation */
         if( lLen >= lInitBuff )
         {
            /* data right truncated! */
            lBuffLen = lLen;
            bOut = ( char * ) hb_xgrab( ( HB_SIZE ) lBuffLen + 1 );
            hb_strncpy( ( char * ) bOut, ( char * ) bBuffer, lLen );
            lLen = lLen - lInitBuff + 2;
            bBuffer = ( char * ) hb_xrealloc( bBuffer, ( HB_SIZE ) lLen );
            iReallocs++;
         }
         else
         {
            hb_storclen( ( char * ) bBuffer, ( HB_SIZE ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
            break;
         }
      }
      else if( ( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO ) && iReallocs > 0 )
      {
         hb_strncat( ( char * ) bOut, ( char * ) bBuffer, lBuffLen );
         hb_storclen( ( char * ) bOut, ( HB_SIZE ) ( lLen + lInitBuff - 1 ), 5 );
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

HB_FUNC( SQLNUMRESULTCOLS ) /* hStmt, @nColCount --> nRetCode */
{
   SQLSMALLINT nCols;
   SQLRETURN result = SQLNumResultCols( ( SQLHSTMT ) hb_parptr( 1 ), &nCols );

/* if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO ) */
      hb_stornl( ( long ) nCols, 2 );

   hb_retni( result );
}

HB_FUNC( SQLDESCRIBECOL ) /* hStmt, nCol, @cName, nLen, @nBufferLen, @nDataType, @nColSize, @nDec, @nNull --> nRetCode */
{
   SQLSMALLINT lLen      = ( SQLSMALLINT ) hb_parni( 4 );
   SQLSMALLINT wBufLen   = ( SQLUSMALLINT ) hb_parni( 5 );
   SQLSMALLINT wDataType = ( SQLUSMALLINT ) hb_parni( 6 );
   SQLULEN     wColSize  = ( SQLULEN ) hb_parnint( 7 );
   SQLSMALLINT wDecimals = ( SQLUSMALLINT ) hb_parni( 8 );
   SQLSMALLINT wNullable = ( SQLUSMALLINT ) hb_parni( 9 );
   SQLTCHAR *  buffer    = ( SQLTCHAR * ) hb_xgrab( lLen * sizeof( SQLTCHAR ) );
   SQLRETURN   result;

   result = SQLDescribeCol( ( SQLHSTMT ) hb_parptr( 1 ),
                            ( SQLUSMALLINT ) hb_parni( 2 ),
                            ( SQLTCHAR * ) buffer,
                            ( SQLSMALLINT ) lLen,
                            ( SQLSMALLINT * ) &wBufLen,
                            ( SQLSMALLINT * ) &wDataType,
                            ( SQLULEN * ) &wColSize,
                            ( SQLSMALLINT * ) &wDecimals,
                            ( SQLSMALLINT * ) &wNullable );

   if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
   {
      O_HB_STORSTRLEN( ( O_HB_CHAR * ) buffer, ( HB_SIZE ) wBufLen, 3 );
      hb_storni( ( int ) wBufLen, 5 );
      hb_storni( ( int ) wDataType, 6 );
      hb_stornint( wColSize, 7 );
      hb_storni( ( int ) wDecimals, 8 );
      hb_storni( ( int ) wNullable, 9 );
   }

   hb_xfree( buffer );
   hb_retni( result );
}

HB_FUNC( SQLCOLATTRIBUTE ) /* hStmt, nCol, nField, @cName, nLen, @nBufferLen, @nAttribute --> nRetCode */
{
   void *      bBuffer   = hb_xgrab( ( SQLUSMALLINT ) hb_parni( 5 ) );
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
   SQLINTEGER  wNumPtr   = ( SQLINTEGER ) hb_parnl( 7 );
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
      hb_storclen( ( char * ) bBuffer, ( HB_SIZE ) wBufLen, 4 );
      hb_storni( ( int ) wBufLen, 6 );
      hb_stornint( wNumPtr, 7 );
   }

   hb_xfree( bBuffer );
   hb_retni( result );
}

HB_FUNC( SQLFETCHSCROLL )
{
#if ODBCVER >= 0x0300
   hb_retni( SQLFetchScroll( ( SQLHSTMT ) hb_parptr( 1 ),
                             ( SQLSMALLINT ) hb_parni( 2 ),
                             ( SQLLEN ) hb_parnint( 3 ) ) );
#else
   hb_retni( SQL_ERROR );
#endif
}

HB_FUNC( SQLERROR ) /* hEnv, hDbc, hStmt, @cErrorClass, @nType, @cErrorMsg */
{
   SQLINTEGER lError;
   SQLSMALLINT wLen;
   SQLTCHAR buffer[ 256 ];
   SQLTCHAR szErrorMsg[ SQL_MAX_MESSAGE_LENGTH + 1 ];

   buffer[ 0 ] = '\0';
   szErrorMsg[ 0 ] = '\0';
   wLen = 0;

   hb_retni( SQLError( ( SQLHENV ) hb_parptr( 1 ),
                       ( SQLHDBC ) hb_parptr( 2 ),
                       ( SQLHSTMT ) hb_parptr( 3 ),
                       ( SQLTCHAR * ) buffer,
                       ( SQLINTEGER * ) &lError,
                       ( SQLTCHAR * ) szErrorMsg,
                       ( SQLSMALLINT ) sizeof( szErrorMsg ),
                       ( SQLSMALLINT * ) &wLen ) );

   O_HB_STORSTR( ( O_HB_CHAR * ) buffer, 4 );
   hb_stornl( ( long ) lError, 5 );
   O_HB_STORSTRLEN( ( O_HB_CHAR * ) szErrorMsg, wLen, 6 );
}

HB_FUNC( SQLGETDIAGREC ) /* nHandleType, hHandle, nRecNumber, @cSQLState, @nError, @cErrorMsg */
{
   SQLTCHAR szSQLState[ 5 + 1 ];
   SQLINTEGER lError;
   SQLTCHAR szErrorMsg[ SQL_MAX_MESSAGE_LENGTH + 1 ];
   SQLSMALLINT wLen;

   szSQLState[ 0 ] = '\0';
   szErrorMsg[ 0 ] = '\0';
   wLen = 0;

   hb_retni( SQLGetDiagRec( ( SQLSMALLINT ) hb_parni( 1 ),
                            ( SQLHANDLE ) ( HB_PTRUINT ) hb_parptr( 2 ),
                            ( SQLSMALLINT) hb_parni( 3 ),
                            ( SQLTCHAR * ) szSQLState,
                            ( SQLINTEGER * ) &lError,
                            ( SQLTCHAR * ) szErrorMsg,
                            ( SQLSMALLINT ) sizeof( szErrorMsg ),
                            ( SQLSMALLINT * ) &wLen ) );

   O_HB_STORSTR( ( O_HB_CHAR * ) szSQLState, 4 );
   hb_stornl( ( long ) lError, 5 );
   O_HB_STORSTRLEN( ( O_HB_CHAR * ) szErrorMsg, wLen, 6 );
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
   char bBuffer[ 512 ];
   SQLSMALLINT wLen;
   SQLRETURN result = SQLGetInfo( ( SQLHDBC ) hb_parptr( 1 ),
                                  ( SQLUSMALLINT ) hb_parni( 2 ),
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
                                HB_ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                HB_ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
   hb_retni( SQLSetConnectOption( ( SQLHDBC ) hb_parptr( 1 ),
                                  ( SQLUSMALLINT ) hb_parni( 2 ),
                                  ( SQLULEN ) HB_ISCHAR( 3 ) ? ( SQLULEN ) hb_parc( 3 ) : hb_parnl( 3 ) ) );
#endif
}

HB_FUNC( SQLSETSTMTATTR ) /* hStmt, nOption, uOption --> nRetCode */
{
#if ODBCVER >= 0x0300
   hb_retni( SQLSetStmtAttr( ( SQLHSTMT ) hb_parptr( 1 ),
                             ( SQLINTEGER ) hb_parnl( 2 ),
                             HB_ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                             HB_ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
   hb_retni( SQLSetStmtOption( ( SQLHSTMT ) hb_parptr( 1 ),
                               ( SQLUSMALLINT ) hb_parni( 2 ),
                               ( SQLULEN ) HB_ISCHAR( 3 ) ? ( SQLULEN ) hb_parc( 3 ) : hb_parnl( 3 ) ) );
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
   char bBuffer[ 512 ];
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
   char bBuffer[ 512 ];
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

HB_FUNC( SQLROLLBACK ) /* hEnv, hDbc */
{
   hb_retni( SQLTransact( ( SQLHENV ) hb_parptr( 1 ), ( SQLHDBC ) hb_parptr( 2 ), SQL_ROLLBACK ) );
}

HB_FUNC( SQLPREPARE ) /* hStmt, cStatement --> nRetCode */
{
   void * hStatement;
   hb_retni( SQLPrepare( ( SQLHSTMT ) hb_parptr( 1 ),
                         ( SQLTCHAR * ) O_HB_PARSTRDEF( 2, &hStatement, NULL ),
                         ( SQLINTEGER  ) SQL_NTS ) );
   hb_strfree( hStatement );
}

HB_FUNC( SQLEXECUTE ) /* hStmt --> nRetCode */
{
   hb_retni( SQLExecute( ( SQLHSTMT ) hb_parptr( 1 ) ) );
}

HB_FUNC( SQLEXECUTESCALAR )
{
   SQLHSTMT hStmt;
   SQLRETURN result;

   result = SQLAllocStmt( ( SQLHDBC ) hb_parptr( 2 ), &hStmt );

   if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
   {
      void * hStatement;
      result = SQLExecDirect( ( SQLHSTMT ) hStmt,
                              ( SQLTCHAR * ) O_HB_PARSTRDEF( 1, &hStatement, NULL ),
                              ( SQLINTEGER ) SQL_NTS );
      hb_strfree( hStatement );

      if( result == SQL_SUCCESS || result == SQL_SUCCESS_WITH_INFO )
      {
         result = SQLFetch( ( SQLHSTMT ) hStmt );
         if( result != SQL_NO_DATA )
         {
            char bBuffer[ 256 ];
            SQLLEN lLen;
            result = SQLGetData( ( SQLHSTMT ) hStmt,
                                 ( SQLUSMALLINT ) 1,
                                 ( SQLSMALLINT ) SQL_C_CHAR,
                                 ( SQLPOINTER ) bBuffer,
                                 ( SQLLEN ) sizeof( bBuffer ),
                                 ( SQLLEN * ) &lLen );
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

HB_FUNC( SQLBINDPARAMETER ) /* nStatementHandle, nParameterNumber, nParameterType, ColumnSize, DecimalDigits, @ParamValue, @ParamLength --> nRetCode */
{
   SQLLEN lLen = ( SQLLEN ) hb_parnint( 7 );
   hb_retni( SQLBindParameter( ( SQLHSTMT ) hb_parptr( 1 ),
                               ( SQLUSMALLINT ) hb_parni( 2 ),
                               ( SQLSMALLINT ) SQL_PARAM_OUTPUT,
                               ( SQLSMALLINT ) SQL_CHAR,
                               ( SQLSMALLINT ) hb_parni( 3 ),
                               ( SQLULEN ) hb_parnint( 4 ),
                               ( SQLSMALLINT ) hb_parni( 5 ),
                               ( SQLPOINTER ) hb_parcx( 6 ),
                               ( SQLLEN ) hb_parclen( 6 ),
                               ( SQLLEN * ) &lLen ) );
   hb_stornint( lLen, 7 );
}

HB_FUNC( HB_ODBCSTOD )
{
   if( hb_parclen( 1 ) >= 10 )
   {
      const char * szSqlDate = hb_parc( 1 );  /* YYYY-MM-DD */
      char szHrbDate[ 9 ];                    /* YYYYMMDD */

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
