/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * NULL RDD
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * www - http://www.harbour-project.org
 *
 * This program is hb_xfree software; you can redistribute it and/or modify
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

/* Harbour without RDD */

ANNOUNCE RDDSYS

#pragma BEGINDUMP

#include "hbrddwrk.h"

HB_FUNC ( NETERR ) { hb_retl( FALSE ); }

HB_FUNC ( FIELDPOS ) { hb_retni( 0 ); }

HB_FUNC( DBRELATION ) { hb_retc( NULL ); }

HB_FUNC( DBRSELECT ) { hb_retni( 0 ); }

HB_FUNC( ORDNAME ) { hb_retc( NULL ); }

HB_FUNC( INDEXORD ) { hb_parni(1); }

HB_FUNC( RDDNAME ) { hb_retc( NULL ); }

HB_FUNC( RDDLIST ) { hb_reta( 0 ); }

HB_FUNC( DBCREATE ) {}

HB_FUNC( DBCLOSEAREA ) {}

HB_FUNC( ALIAS ) { hb_retc( NULL ); }

HB_FUNC( DBAPPEND ) {}

HB_FUNC( DBSKIP ) { hb_retni( 0 ); }

HB_FUNC( FIELDGET ) { hb_retc( NULL ); }

HB_FUNC( FIELDPUT ) { hb_retc( NULL ); }

HB_FUNC( DBUNLOCK ) { hb_retl( FALSE ); }

HB_FUNC( DBSKIPPER ) { hb_retni( 0 ); }

HB_FUNC( DBSEEK ) { hb_retl( FALSE ); }

HB_FUNC( DBGOTO ) { hb_retni( 0 ); }

HB_FUNC( INDEXKEY ) { hb_retc( NULL ); }

HB_FUNC( DBGOTOP ) {}

HB_FUNC( DBGOBOTTOM ) {}

HB_FUNC( USED ) { hb_retl( FALSE ); }

HB_FUNC( SELECT ) { hb_retni( 0 ); }

HB_FUNC( LOCK ) { hb_retl( FALSE ); }

HB_FUNC( LASTREC ) { hb_retni( 0 ); }

HB_FUNC( FOUND ) { hb_retl( FALSE ); }

HB_FUNC( FLOCK ) { hb_retl( FALSE ); }

HB_FUNC( DELETED ) { hb_retl( FALSE ); }

HB_FUNC( BOF ) { hb_retl( FALSE ); }

HB_FUNC( FIELDNAME ) { hb_retc( NULL ); }

HB_FUNC( RECCOUNT ) { hb_parni( 0 ); }

HB_FUNC( FCOUNT ) { hb_parni( 0 ); }

HB_FUNC( RLOCK ) { hb_retl( FALSE ); }

HB_FUNC( RECNO ) { hb_retni( 0 ); }

HB_FUNC( EOF ) { hb_retl( FALSE ); }

void hb_rddShutDown( void ) {}

ERRCODE  HB_EXPORT hb_rddFieldPut( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_SYMBOL_UNUSED( pItem );
   HB_SYMBOL_UNUSED( pFieldSymbol );

   return FAILURE;
}

ERRCODE  HB_EXPORT hb_rddSelectWorkAreaSymbol( PHB_SYMB pSymAlias )
{
   HB_SYMBOL_UNUSED( pSymAlias );

   return FAILURE;
}

ERRCODE  HB_EXPORT hb_rddSelectWorkAreaAlias( char * szName )
{
   HB_SYMBOL_UNUSED( szName );

   return FAILURE;
}

ERRCODE  HB_EXPORT hb_rddGetFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_SYMBOL_UNUSED( pItem );
   HB_SYMBOL_UNUSED( pFieldSymbol );

   return FAILURE;
}

ERRCODE  HB_EXPORT hb_rddFieldGet( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_SYMBOL_UNUSED( pItem );
   HB_SYMBOL_UNUSED( pFieldSymbol );

   return FAILURE;
}

ERRCODE  HB_EXPORT hb_rddSelectWorkAreaNumber( int iArea )
{
   HB_SYMBOL_UNUSED( iArea );

   return FAILURE;
}

ERRCODE  HB_EXPORT hb_rddPutFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_SYMBOL_UNUSED( pItem );
   HB_SYMBOL_UNUSED( pFieldSymbol );

   return FAILURE;
}

int      HB_EXPORT hb_rddGetCurrentWorkAreaNumber( void )
{
   return 0;
}
#pragma ENDDUMP
