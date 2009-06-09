/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * mSQL DBMS low level (client api) interface code.
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

#include "hbapi.h"
#include "hbapiitm.h"

#if defined( HB_OS_WIN ) && !defined( _OS_WIN32 )
   /* This is needed by msql.h to indicate the platform. */
   #define _OS_WIN32
#endif

#include "msql.h"

#define HB_PARPTR( n ) hb_parptr( n )
#define HB_RETPTR( n ) hb_retptr( n )

HB_FUNC( MSQLCONNEC ) /* int msqlConnect(char *) */
{
   hb_retni( msqlConnect( hb_parc( 1 ) ) );
}

HB_FUNC( MSQLCLOSE ) /* void msqlClose(int) */
{
   msqlClose( hb_parni( 1 ) );
}

HB_FUNC( MSQLSELECT ) /* int msqlSelectDB(int, char *) */
{
   hb_retni( msqlSelectDB( hb_parni( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( MSQLQUERY ) /* int msqlQuery(int, char *) */
{
   hb_retni( msqlQuery( hb_parni( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( MSQLSTORER ) /* m_result *msqlStoreResult() */
{
   HB_RETPTR( ( void * ) msqlStoreResult() );
}

HB_FUNC( MSQLFREER ) /* void msqlFreeResult(m_result *) */
{
   msqlFreeResult( ( m_result * ) HB_PARPTR( 1 ) );
}

/* NOTE: need number of retrieved fields */
HB_FUNC( MSQLFETCHR ) /* m_row msqlFetchRow(m_result *, int) */
{
   m_result * mresult = ( m_result * ) HB_PARPTR( 1 );
   int num_fields = hb_parnl( 2 );
   PHB_ITEM aRow = hb_itemArrayNew( num_fields );
   m_row mrow = msqlFetchRow( mresult );
   int i;

   for( i = 0; i < num_fields; i++ )
      hb_arraySetC( aRow, i + 1, mrow[ i ] );

   hb_itemReturnRelease( aRow );
}
 
HB_FUNC( MSQLDATASE ) /* void msqlDataSeek(m_result *, int) */
{
   msqlDataSeek( ( m_result * ) HB_PARPTR( 1 ), hb_parni( 2 ) );
}

HB_FUNC( MSQLNUMROW ) /* int msqlNumRows(m_result *) */
{
   hb_retni( msqlNumRows( ( ( m_result * ) HB_PARPTR( 1 ) ) ) );
}

HB_FUNC( MSQLFETCHF ) /* m_field *msqlFetchField(m_result *) */
{
   m_field * mfield = msqlFetchField( ( m_result * ) HB_PARPTR( 1 ) );
   PHB_ITEM aField = hb_itemArrayNew( 5 ); /* NOTE: m_field structure of mSQL 2.x has 5 members */

   if( mfield )
   {
      hb_arraySetC(  aField, 1, mfield->name );
      hb_arraySetC(  aField, 2, mfield->table );
      hb_arraySetNL( aField, 3, mfield->type );
      hb_arraySetNL( aField, 4, mfield->length );
      hb_arraySetNL( aField, 5, mfield->flags );
   }

   hb_itemReturnRelease( aField );
}

HB_FUNC( MSQLFIELDS ) /* void msqlFieldSeek(m_result *, int) */
{
   msqlFieldSeek( ( m_result * ) HB_PARPTR( 1 ), hb_parni( 2 ) );
}

HB_FUNC( MSQLNUMFIE ) /* int msqlNumFields(m_result *) */
{
   hb_retni( msqlNumFields( ( ( m_result * ) HB_PARPTR( 1 ) ) ) );
}

HB_FUNC( MSQLLISTFI ) /* m_result *msqlListFields(int, char *); */
{
   HB_RETPTR( ( void * ) msqlListFields( hb_parni( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( MSQLGETERR ) /* char *msqlGetErrMsg(char *); */
{
   hb_retc( msqlErrMsg );
}

HB_FUNC( MSQLLISTDB ) /* m_result * msqlListDBs(int); */
{
   int sock = hb_parnl( 1 );
   m_result * mresult = msqlListDBs( sock );
   long nr = msqlNumRows( mresult );
   PHB_ITEM aDBs = hb_itemArrayNew( nr );
   long i;

   for( i = 0; i < nr; i++ )
      hb_arraySetC( aDBs, i + 1, msqlFetchRow( mresult )[ 0 ] );

   msqlFreeResult( mresult );
   hb_itemReturnRelease( aDBs );
}

HB_FUNC( MSQLLISTTA ) /* m_result * msqlListTables(int); */
{
   int sock = hb_parnl( 1 );
   m_result * mresult = msqlListTables( sock );
   long nr = msqlNumRows( mresult );
   PHB_ITEM aTables = hb_itemArrayNew( nr );
   long i;

   for( i = 0; i < nr; i++ )
      hb_arraySetC( aTables, i + 1, msqlFetchRow( mresult )[ 0 ] );

   msqlFreeResult( mresult );
   hb_itemReturnRelease( aTables );
}
