/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PROCNAME(), PROCLINE() and PROCFILE() functions
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
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    PROCFILE()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbstack.h"

HB_FUNC( PROCNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 2 ];

   hb_retc( hb_procname( hb_parni( 1 ) + 1, szName ) );
}

HB_FUNC( PROCLINE )
{
   int iLevel = hb_parni( 1 ) + 1;  /* we are already inside ProcName() */
   PHB_ITEM * pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + ( *pBase )->item.asSymbol.stackbase;

   if( iLevel == -1 )
      hb_retni( ( *pBase )->item.asSymbol.lineno );
   else
      hb_retni( 0 );
}

#ifdef HB_C52_UNDOC

/* NOTE: Clipper undocumented function, which always returns an empty
         string. [vszakats] */

HB_FUNC( PROCFILE )
{
   hb_retc( "" );
}

#endif

/* NOTE: szName size must be an at least:
         HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 2 [vszakats] */

char * hb_procname( int iLevel, char * szName )
{
   PHB_ITEM * pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + ( *pBase )->item.asSymbol.stackbase;

   if( iLevel == -1 )
   {
      if( ( *( pBase + 1 ) )->type == HB_IT_ARRAY )  /* it is a method name */
      {
         strcpy( szName, hb_objGetClsName( *( pBase + 1 ) ) );
         strcat( szName, ":" );
         strcat( szName, ( *pBase )->item.asSymbol.value->szName );
      }
      else
         strcpy( szName, ( *pBase )->item.asSymbol.value->szName );
   }
   else
      strcpy( szName, "" );

   return szName;
}

