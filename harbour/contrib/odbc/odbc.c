/*
 * $Id$
 */

/*
 * Harbour Project source code
 *
 * This file contains source for first odbc routines.
 *
 * Copyright 1999  Antonio Linares <alinares@fivetech.com>
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
 *    HB_SQLNUMRES()
 *    HB_SQLDESCRIB()
 *    HB_SQLEXTENDE()
 *
 * See doc/license.txt for licensing terms.
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org
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
 * See doc/license.txt for licensing terms.
 */

#include "hbapi.h"

#if !defined(HB_OS_DOS) && !defined(HB_OS_OS2)

#if defined(HB_OS_WIN_32)
   #include <windows.h>
#endif

#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>

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
   #ifdef _WIN64
      typedef INT64           SQLLEN;
   #else
      #define SQLLEN          SQLINTEGER
   #endif
#endif

#if defined(__DMC__)
   #define SQL_NO_DATA SQL_NO_DATA_FOUND
   #define SQLColAttribute  SQLColAttributes
   SQLRETURN  SQL_API SQLFetchScroll(SQLHSTMT StatementHandle,
              SQLSMALLINT FetchOrientation, SQLINTEGER FetchOffset);
#endif

HB_FUNC( SQLALLOCEN ) /* HB_SQLALLOCENV( @hEnv ) --> nRetCode */
{
   HENV hEnv;
   RETCODE ret = SQLAllocEnv( &hEnv );

   hb_stornl( ( LONG ) hEnv, 1 );
   hb_retni( ret );
}

HB_FUNC( SQLALLOCCO ) /* HB_SQLALLOCCONNECT( hEnv, @ hDbc ) --> nRetCode */
{
   HDBC hDbc;
   RETCODE ret = SQLAllocConnect( ( HENV ) hb_parnl( 1 ), &hDbc );

   hb_stornl( ( LONG ) hDbc, 2 );
   hb_retni( ret );
}

HB_FUNC( SQLDRIVERC ) /* HB_SQLDRIVERCONNECT( hDbc, @ cConnectString ) --> nRetCode */
{
   SWORD  wLen;
   RETCODE ret;

#if defined( HB_WINCE )
   LPTSTR lpStr = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   TCHAR buffer[ 1024 ];
   buffer[ 0 ] = '\0';
   ret =  SQLDriverConnect( ( HDBC ) hb_parnl( 1 ),
                            GetDesktopWindow(), lpStr, hb_parclen( 2 ),
                            buffer, sizeof( buffer ), &wLen,
                            SQL_DRIVER_COMPLETE );
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
   ret =  SQLDriverConnect( ( HDBC ) hb_parnl( 1 ),
                            0, ( SQLCHAR * ) hb_parcx( 2 ), hb_parclen( 2 ),
                            buffer, sizeof( buffer ), &wLen,
                            SQL_DRIVER_COMPLETE );
   hb_storc( ( char * ) buffer, 3 );
#endif
   hb_retni( ret );
}

HB_FUNC( SQLCONNECT ) /* HB_SQLCONNECT( hDbc, cDSN, cUseName, cPassword ) --> nRetCode */
{
   RETCODE ret;
#if defined( HB_WINCE )
   LPTSTR lpDSN      = HB_TCHAR_CONVTO( hb_parcx( 2 ) ),
          lpUseName  = HB_TCHAR_CONVTO( hb_parcx( 3 ) ),
          lpPassword = HB_TCHAR_CONVTO( hb_parcx( 4 ) );

   ret =  SQLConnect( ( HDBC ) hb_parnl( 1 ),
                      lpDSN,
                      hb_parclen( 2 ),
                      lpUseName,
                      hb_parclen( 3 ),
                      lpPassword,
                      hb_parclen( 4 ) );

   HB_TCHAR_FREE( lpDSN );
   HB_TCHAR_FREE( lpUseName );
   HB_TCHAR_FREE( lpPassword );
#else
   ret =  SQLConnect( ( HDBC ) hb_parnl( 1 ),
                      (unsigned char*) hb_parcx( 2 ),
                      hb_parclen( 2 ),
                      (unsigned char*) hb_parcx( 3 ),
                      hb_parclen( 3 ),
                      (unsigned char*) hb_parcx( 4 ),
                      hb_parclen( 4 ) );
#endif
   hb_retni( ret );
}

HB_FUNC( SQLDISCONN )  /* HB_SQLDISCONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLDisconnect( ( HDBC ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLFREECON )  /* HB_SQLFREECONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLFreeConnect( ( HDBC ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLFREEENV )  /* HB_SQLFREEENV( hEnv ) --> nRetCode */
{
   hb_retni( SQLFreeEnv( ( HENV ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLALLOCST )  /* HB_SQLALLOCSTMT( hDbc, @ hStmt ) --> nRetCode */
{
   HSTMT hStmt;

   hb_retni( SQLAllocStmt( ( HDBC ) hb_parnl( 1 ), &hStmt ) );
   hb_stornl( ( LONG ) hStmt, 2 );
}

HB_FUNC( SQLFREESTM ) /* HB_SQLFREESTMT( hStmt, nType ) --> nRetCode */
{
   hb_retni( SQLFreeStmt( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( SQLEXECDIR )  /* HB_SQLEXECDIRECT( hStmt, cStatement ) --> nRetCode */
{
#if defined( HB_WINCE )
   LPTSTR lpStr = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   hb_retni( SQLExecDirect( ( HSTMT ) hb_parnl( 1 ), lpStr, hb_parclen( 2 ) ) );
   HB_TCHAR_FREE( lpStr );
#else
   hb_retni( SQLExecDirect( ( HSTMT ) hb_parnl( 1 ), (unsigned char*) hb_parcx( 2 ), hb_parclen( 2 ) ) );
#endif
}

HB_FUNC( SQLFETCH )   /* HB_SQLFETCH( hStmt ) --> nRetCode */
{
   hb_retni( SQLFetch( ( HSTMT ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLGETDATA ) /* HB_SQLGETDATA( hStmt, nField, nType, nLen, @cBuffer ) --> nRetCode */
{
   SDWORD lLen, lInitBuff;
   PTR  bBuffer, bOut;
   WORD wType, wResult;
   int iReallocs = 0;

   lLen       = ( SDWORD )( hb_parnl( 4 ) ? hb_parnl( 4 ) : 64 );
   bBuffer    = hb_xgrab( (ULONG) lLen+1 );
   bOut       = NULL;
   lInitBuff  = lLen;
   wType      = ( hb_parni( 3 ) ? hb_parni( 3 ) : SQL_BINARY );

   wResult = ! SQL_NO_DATA;
   while( wResult != SQL_NO_DATA )
   {
      wResult    = SQLGetData( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ), wType, ( PTR ) bBuffer, lLen, &lLen );
      if( wResult == SQL_SUCCESS && iReallocs == 0 )
      {
         hb_storclen( ( LPSTR ) bBuffer, ( ULONG ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
         break;
      }
      else if ( wResult == SQL_SUCCESS_WITH_INFO && iReallocs == 0 )
      {
         /* Perheps a data truncation */
         if( lLen >= lInitBuff )
         {
            /* data right truncated! */
            bOut    = ( char * ) hb_xgrab( (ULONG) lLen + 1 );
            lLen = lLen - lInitBuff+2;
            strcpy( (char *) bOut, (char *) bBuffer );
            bBuffer = ( char * ) hb_xrealloc( bBuffer, (ULONG) lLen );
         }
         else
         {
            hb_storclen( ( LPSTR ) bBuffer, ( ULONG ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
            break;
         }
         iReallocs++;
      }
      else if( (wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO ) && iReallocs > 0 )
      {
         strcat( (char*) bOut, (char *) bBuffer );
         hb_storclen( ( LPSTR ) bOut, ( ULONG ) ( lLen + lInitBuff - 1 ), 5 );
         wResult = SQL_SUCCESS;
         break;
      }
      else
      {
         break;
      }
   }
   hb_xfree( ( PTR ) bBuffer );
   if( bOut )
   {
      hb_xfree( ( PTR ) bOut );
   }
   hb_retni( wResult );
}

/* HB_NUMRESULTCOLS( hStmt, @nColCount ) */
HB_FUNC( SQLNUMRES )
{
    SQLSMALLINT nCols;
    WORD wResult = SQLNumResultCols( ( HSTMT ) hb_parnl( 1 ), &nCols );

/* if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO ) */
      hb_stornl( ( LONG ) nCols, 2 );

    hb_retni( wResult );
}

/* HB_SQLDESCRIBECOL( hStmt, nCol, @cName, nLen, @nBufferLen, @nDataType, @nColSize, @nDec, @nNull ) --> nRetCode */
HB_FUNC( SQLDESCRIB )
{
    SDWORD      lLen      = ( SDWORD ) hb_parnl( 4 );
    SQLSMALLINT wBufLen   = hb_parni( 5 );
    SQLSMALLINT wDataType = hb_parni( 6 );
    SQLUINTEGER wColSize  = hb_parni( 7 );
    SQLSMALLINT wDecimals = hb_parni( 8 );
    SQLSMALLINT wNullable = hb_parni( 9 );
#if defined( HB_WINCE )
    LPTSTR      buffer    = ( LPTSTR ) hb_xgrab( lLen * sizeof( TCHAR ) );
#else
    SQLCHAR *   buffer    = ( SQLCHAR * ) hb_xgrab( lLen * sizeof( SQLCHAR ) );
#endif
    WORD        wResult;

    wResult = SQLDescribeCol( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ),
                              buffer, lLen, &wBufLen,
                              &wDataType, &wColSize, &wDecimals,
                              &wNullable );

    if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
    {
       if( ISBYREF( 3 ) )
       {
#if defined( HB_WINCE )
         char * szStr = HB_TCHAR_CONVFROM( buffer );
         hb_storc( szStr, 3 );
         HB_TCHAR_FREE( szStr );
#else
         hb_storclen( ( char * ) buffer, ( WORD ) wBufLen, 3 );
#endif
       }
       hb_stornl( ( LONG ) wBufLen, 5 );
       hb_stornl( ( LONG ) wDataType, 6 );
       hb_stornl( ( LONG ) wColSize, 7 );
       hb_stornl( ( LONG ) wDecimals, 8 );
       hb_stornl( ( LONG ) wNullable, 9 );
    }

    hb_xfree( buffer );
    hb_retni( wResult );
}

/* SQLCOLATTRIBUTE( hStmt, nCol, nField, @cName, nLen, @nBufferLen, @nAttribute ) --> nRetCode */
HB_FUNC( SQLCOLATTRIBUTE )
{
    SDWORD      lLen      = ( SDWORD ) hb_parnl( 5 );
    PTR         bBuffer   = hb_xgrab( lLen );
    SQLSMALLINT wBufLen   = hb_parni( 6 );
    SQLSMALLINT wNumPtr   = hb_parni( 7 );
    WORD        wResult   = SQLColAttribute( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                                             (unsigned char*) bBuffer, hb_parni( 5 ), &wBufLen,
#if defined(__DMC__)
                                             (SQLINTEGER FAR*) &wNumPtr );
#else
                                             (SQLPOINTER) &wNumPtr );
#endif

    if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
    {
       hb_storclen( ( LPSTR ) bBuffer,
                    ( WORD ) wBufLen, 4 );
       hb_stornl( ( LONG ) wBufLen, 6 );
       hb_stornl( ( LONG ) wNumPtr, 7 );
    }

    hb_xfree( ( PTR ) bBuffer );
    hb_retni( wResult );
}

/* HB_SQLEXTENDEDFETCH( hStmt, nOrientation, nOffset, @nRows, @nRowStatus ) */
HB_FUNC( SQLEXTENDE )
{
    SQLUINTEGER  uiRowCountPtr = hb_parni( 4 );
    SQLUSMALLINT siRowStatus   = hb_parni( 5 );
    WORD         wResult       = SQLExtendedFetch( ( HSTMT ) hb_parnl( 1 ),
                                                   ( USHORT )hb_parnl( 2 ),
                                                   ( USHORT )hb_parnl( 3 ),
                                                   &uiRowCountPtr,
                                                   &siRowStatus );

    if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
    {
       hb_stornl( ( LONG ) uiRowCountPtr, 4 );
       hb_stornl( ( LONG ) siRowStatus, 5 );
    }

    hb_retni( wResult );
}

HB_FUNC( SQLFETCHSC )
{
    hb_retni( SQLFetchScroll( ( HSTMT ) hb_parnl( 1 ),
                              ( SHORT ) hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC( SQLERROR ) //  hEnv, hDbc, hStmt, @ cErrorClass, @ nType, @ cErrorMsg
{
   SQLINTEGER lError;
   SWORD      wLen;
#if defined( HB_WINCE )
   TCHAR      buffer[ 256 ], szErrorMsg[ 256 ];
#else
   BYTE       buffer[ 256 ], szErrorMsg[ 256 ];
#endif
   hb_retni( SQLError( ( HENV ) hb_parnl( 1 ), ( HDBC ) hb_parnl( 2 ),
                       ( HSTMT ) hb_parnl( 3 ), buffer, &lError,
                       szErrorMsg, 256, &wLen ) );

   if( ISBYREF( 4 ) )
   {
#if defined( HB_WINCE )
      char * szStr = HB_TCHAR_CONVFROM( buffer );
      hb_storc( szStr, 4 );
      HB_TCHAR_FREE( szStr );
#else
      hb_storc( (char *) buffer, 4 );
#endif
   }
   hb_stornl( ( LONG ) lError, 5 );
   if( ISBYREF( 6 ) )
   {
#if defined( HB_WINCE )
      char * szStr = HB_TCHAR_CONVFROM( szErrorMsg );
      hb_storc( szStr, 6 );
      HB_TCHAR_FREE( szStr );
#else
      hb_storc( (char *) szErrorMsg, 6 );
#endif
   }
}

HB_FUNC( SQLROWCOUN )
{
    SQLLEN  iRowCountPtr = hb_parni( 2 );
    WORD    wResult      = SQLRowCount( ( HSTMT ) hb_parnl( 1 ),
                                        &iRowCountPtr );
    if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
    {
       hb_stornl( ( LONG ) iRowCountPtr, 2 );
    }

    hb_retni( wResult );
}

HB_FUNC( SQLGETINFO ) // hDbc, nType, @cResult
{
   BYTE bBuffer[ 512 ];
   SQLSMALLINT wLen;
   WORD wResult = SQLGetInfo( ( HDBC ) hb_parnl( 1 ), ( UWORD ) hb_parnl( 2 ), bBuffer, 512, &wLen );

   hb_storclen( (char *) bBuffer, wLen, 3 );
   hb_retni( wResult );
}

HB_FUNC( SQLSETCONNECTOPTION ) // hDbc, nOption, uOption
{
   hb_retnl( ( LONG ) SQLSetConnectOption( ( HDBC ) hb_parnl( 1 ), ( UWORD ) hb_parnl( 2 ),
           ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parcx( 3 ) : hb_parnl( 3 ) ) );
}

HB_FUNC( SQLSETSTMTOPTION ) // hStmt, nOption, uOption )  --> nRetCode
{
   hb_retnl( ( LONG ) SQLSetStmtOption( ( SQLHSTMT ) hb_parnl( 1 ), ( UWORD ) hb_parnl( 2 ),
           ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parcx( 3 ) : hb_parnl( 3 ) ) );
}

HB_FUNC( SQLGETCONNECTOPTION ) // hDbc, nOption, @cOption
{
   BYTE bBuffer[ 512 ];
   WORD wResult = SQLGetConnectOption( ( HDBC ) hb_parnl( 1 ), hb_parni( 2 ), bBuffer );
   if( wResult == SQL_SUCCESS )
      hb_storclen( (char *) bBuffer, 512, 3 );

   hb_retni( wResult );
}

HB_FUNC( SQLGETSTMTOPTION ) // hStmt, nOption, @cOption
{
   BYTE bBuffer[ 512 ];
   WORD wResult = SQLGetStmtOption( ( SQLHSTMT ) hb_parnl( 1 ), hb_parni( 2 ), bBuffer );

   if( wResult == SQL_SUCCESS )
   {
      hb_storclen( (char *) bBuffer, 512,3 );
   }

   hb_retni( wResult );
}

HB_FUNC( SQLCOMMIT ) // hEnv, hDbc
{
   hb_retni( SQLTransact( ( HENV ) hb_parnl( 1 ), ( HDBC ) hb_parnl( 2 ), SQL_COMMIT ) );
}

HB_FUNC( SQLROLLBACK )  // hEnv, hDbc
{
   hb_retni( SQLTransact( ( HENV ) hb_parnl( 1 ), ( HDBC ) hb_parnl( 2 ), SQL_ROLLBACK ) );
}
HB_FUNC( SETNUMLEN )  /* SETNUMLEN( nValue, nSize, nDecimals ) ==> nValue (nSize, nDec) */
{
   hb_retnlen( hb_parnd( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) );
}

HB_FUNC( SQLPREPARE )  /* HB_SQLPREPARE( hStmt, cStatement ) --> nRetCode */
{
#if defined( HB_WINCE )
   LPTSTR lpStr = HB_TCHAR_CONVTO( hb_parcx( 2 ) );
   hb_retni( SQLPrepare( ( HSTMT ) hb_parnl( 1 ), lpStr, SQL_NTS ) );
   HB_TCHAR_FREE( lpStr );
#else
   hb_retni( SQLPrepare( ( HSTMT ) hb_parnl( 1 ), (unsigned char*) hb_parcx( 2 ), SQL_NTS ) );
#endif
}

HB_FUNC( SQLEXECUTE )  /* HB_SQLEXECUTE( hStmt ) --> nRetCode */
{
   hb_retni( SQLExecute( ( HSTMT ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLEXECUTESCALAR )
{
   HSTMT hStmt;
   SDWORD lLen;
   BYTE bBuffer[ 256 ];
   SWORD wResult;

   wResult = SQLAllocStmt( ( HDBC ) hb_parnl( 2 ), &hStmt );

   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
   {
#if defined( HB_WINCE )
      LPTSTR lpStr = HB_TCHAR_CONVTO( hb_parcx( 1 ) );
      wResult = SQLExecDirect( ( HSTMT ) hStmt, lpStr, SQL_NTS );
      HB_TCHAR_FREE( lpStr );
#else
      wResult = SQLExecDirect( ( HSTMT ) hStmt, (unsigned char*) hb_parcx( 1 ), SQL_NTS );
#endif
      if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
      {
         wResult = SQLFetch( ( HSTMT ) hStmt );
         if( wResult != SQL_NO_DATA )
         {
            wResult = SQLGetData( ( HSTMT ) hStmt, 1, SQL_C_CHAR, bBuffer, sizeof( bBuffer ), &lLen );
            hb_storc( (char *)bBuffer, 3 );
         }
      }
   }

   hb_retni( wResult );

   SQLFreeStmt( ( HSTMT ) hStmt, 0 );

}

HB_FUNC( SQLSTOD )
{
   if( hb_parclen( 1 ) >= 10 )
   {
      char *szSqlDate = hb_parc( 1 );  /* YYYY-MM-DD */
      char szHrbDate[9];               /* YYYYMMDD */

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
   {
      hb_retds( NULL );
   }
}

HB_FUNC( SQLMORERESULTS ) // hEnv, hDbc
{
   hb_retni( SQLMoreResults( ( SQLHSTMT ) hb_parnl( 1 ) ) );
}

#if 0
HB_FUNC( SQLBINDOUTPARAM ) /* SqlBindOutParam( nStatementHandle, nParameterNumber, nParameterType, ColumnSize, DecimalDigits, @ParamValue, @ParamLength    ) --> nRetCode */
{
   SQLLEN lLen      = hb_parnl( 7 );
   RETCODE ret;

   ret = SQLBindParameter( ( HSTMT ) hb_parnl( 1 ), (USHORT) hb_parni( 2 ),
                           SQL_PARAM_OUTPUT, SQL_CHAR, (USHORT) hb_parni( 3 ),
                           (USHORT) hb_parni( 4 ), (USHORT) hb_parni( 5 ),
                           hb_parcx( 6 ), hb_parclen( 6 ),
                           &lLen );
   hb_stornl( ( LONG ) lLen, 7 );
   hb_retni( ret );
}
#endif

#endif
