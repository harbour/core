/*
 * $Id$
 *
   Harbour Project source code

   This file contains source for first odbc routines.

   Copyright (C) 1999 Antonio Linares
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

*/

#include <windows.h>
#include <limits.h>
#include <malloc.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "init.h"
#include "harb.h"
#include <sql.h>
#include <sqlext.h>

/* ODBC32 */
HARBOUR HB_SQLALLOCEN( void );
HARBOUR HB_SQLALLOCCO( void );
HARBOUR HB_SQLDRIVERC( void );
HARBOUR HB_SQLDISCONN( void );
HARBOUR HB_SQLFREECON( void );
HARBOUR HB_SQLFREEENV( void );
HARBOUR HB_SQLALLOCST( void );
HARBOUR HB_SQLFREESTM( void );
HARBOUR HB_SQLEXECDIR( void );
HARBOUR HB_SQLFETCH( void );
HARBOUR HB_SQLGETDATA( void );

HB_INIT_SYMBOLS_BEGIN( odbc__InitSymbols )
{ "SQLALLOCEN",     FS_PUBLIC, HB_SQLALLOCEN    , 0 },
{ "SQLALLOCCO",     FS_PUBLIC, HB_SQLALLOCCO    , 0 },
{ "SQLDRIVERC",     FS_PUBLIC, HB_SQLDRIVERC    , 0 },
{ "SQLDISCONN",     FS_PUBLIC, HB_SQLDISCONN    , 0 },
{ "SQLFREECON",     FS_PUBLIC, HB_SQLFREECON    , 0 },
{ "SQLFREEENV",     FS_PUBLIC, HB_SQLFREEENV    , 0 },
{ "SQLALLOCST",     FS_PUBLIC, HB_SQLALLOCST    , 0 },
{ "SQLFREESTM",     FS_PUBLIC, HB_SQLFREESTM    , 0 },
{ "SQLEXECDIR",     FS_PUBLIC, HB_SQLEXECDIR    , 0 },
{ "SQLFETCH",       FS_PUBLIC, HB_SQLFETCH      , 0 },
{ "SQLGETDATA",     FS_PUBLIC, HB_SQLGETDATA    , 0 }
HB_INIT_SYMBOLS_END( odbc__InitSymbols );
#pragma odbc__InitSymbols

HARBOUR HB_SQLALLOCEN( void ) /* HB_SQLALLOCENV( @hEnv ) --> nRetCode */
{
   HENV hEnv;
   RETCODE ret = SQLAllocEnv( &hEnv );

   hb_stornl( ( LONG ) hEnv, 1 );
   hb_retni( ret );
}

HARBOUR HB_SQLALLOCCO( void ) /* HB_SQLALLOCCONNECT( hEnv, @ hDbc ) --> nRetCode */
{
   HDBC hDbc;
   RETCODE ret = SQLAllocConnect( ( HENV ) hb_parnl( 1 ), &hDbc );

   hb_stornl( ( LONG ) hDbc, 2 );
   hb_retni( ret );
}

HARBOUR HB_SQLDRIVERC( void ) /* HB_SQLDRIVERCONNECT( hDbc, @ cConnectString, lPrompt ) --> nRetCode */
{
   BYTE  bBuffer1[ 1024 ];
   SWORD  wLen;
   RETCODE ret =  SQLDriverConnect( ( HDBC ) hb_parnl( 1 ),
                             GetDesktopWindow(),
                             hb_parc( 2 ), strlen(hb_parc(2)),
                             bBuffer1, 1024, &wLen, SQL_DRIVER_COMPLETE ) ;
   hb_storc( bBuffer1 , 3 );
   hb_retni( ret );
}

HARBOUR HB_SQLDISCONN( void )  /* HB_SQLDISCONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLDisconnect( ( HDBC ) hb_parnl( 1 ) ) );
}

HARBOUR HB_SQLFREECON( void )  /* HB_SQLFREECONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLFreeConnect( ( HDBC ) hb_parnl( 1 ) ) );
}

HARBOUR HB_SQLFREEENV( void )  /* HB_SQLFREEENV( hEnv ) --> nRetCode */
{
   hb_retni( SQLFreeEnv( ( HENV ) hb_parnl( 1 ) ) );
}

HARBOUR HB_SQLALLOCST()  /* HB_SQLALLOCSTMT( hDbc, @ hStmt ) --> nRetCode */
{
   HSTMT hStmt;

   hb_retni( SQLAllocStmt( ( HDBC ) hb_parnl( 1 ), &hStmt ) );
   hb_stornl( ( LONG ) hStmt, 2 );
}

HARBOUR HB_SQLFREESTM() /* HB_SQLFREESTMT( hStmt, nType ) --> nRetCode */
{
   hb_retni( SQLFreeStmt( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ) ) );
}

HARBOUR HB_SQLEXECDIR( void )  /* HB_SQLEXECDIRECT( hStmt, cStatement ) --> nRetCode */
{
   hb_retni( SQLExecDirect( ( HSTMT ) hb_parnl( 1 ), hb_parc( 2 ), SQL_NTS ) );
}

HARBOUR HB_SQLFETCH( void )   /* HB_SQLFETCH( hStmt ) --> nRetCode */
{
   hb_retni( SQLFetch( ( HSTMT ) hb_parnl( 1 ) ) );
}

HARBOUR HB_SQLGETDATA( void ) /* HB_SQLGETDATA( hStmt, nField, nType, nLen, @cBuffer ) --> nRetCode */
{
   SDWORD lLen  = ( SDWORD ) hb_parnl( 4 );
   PTR  bBuffer = hb_xgrab( lLen );
   WORD wType   = hb_parni( 3 );
   WORD wResult = SQLGetData( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ),
                              wType, ( PTR ) bBuffer, lLen, &lLen );

   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
      hb_storclen( ( LPSTR ) bBuffer,
                 ( WORD ) lLen, 5 );

   hb_xfree( ( PTR ) bBuffer );
   hb_retni( wResult );
}
