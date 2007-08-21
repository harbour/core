/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    HB_COMPILE() - compiler interface
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbcomp.h"

static void hb_compGenArgList( int iFirst, int iLast,
                               int * pArgC, char *** pArgV )
{
   PHB_ITEM pParam, pItem;
   ULONG ul;
   int argc = 0, i;
   char ** argv;

   for( i = iFirst; i <= iLast; ++i )
   {
      pParam = hb_param( i, HB_IT_ARRAY | HB_IT_STRING );
      if( pParam )
      {
         if( HB_IS_ARRAY( pParam ) )
         {
            ul = hb_arrayLen( pParam );
            if( ul ) do
            {
               pItem = hb_arrayGetItemPtr( pParam, ul );
               if( pItem && HB_IS_STRING( pItem ) )
                  ++argc;
            }
            while( --ul );
         }
         else if( HB_IS_STRING( pParam ) )
            ++argc;
      }
   }

   argv = ( char ** ) hb_xgrab( sizeof( char * ) * ( argc + 1 ) );
   argc = 0;
   for( i = iFirst; i <= iLast; ++i )
   {
      pParam = hb_param( i, HB_IT_ARRAY | HB_IT_STRING );
      if( pParam )
      {
         if( HB_IS_ARRAY( pParam ) )
         {
            ul = hb_arrayLen( pParam );
            if( ul ) do
            {
               pItem = hb_arrayGetItemPtr( pParam, ul );
               if( pItem && HB_IS_STRING( pItem ) )
                  argv[ argc++ ] = hb_itemGetCPtr( pItem );
            }
            while( --ul );
         }
         else if( HB_IS_STRING( pParam ) )
            argv[ argc++ ] = hb_itemGetCPtr( pParam );
      }
   }
   argv[ argc ] = NULL;

   * pArgC = argc;
   * pArgV = argv;
}

HB_FUNC( HB_COMPILE )
{
   int argc;
   char ** argv;

   hb_compGenArgList( 1, hb_pcount(), &argc, &argv );

   hb_retni( hb_compMain( argc, argv, NULL, NULL ) );
   hb_xfree( argv );
}

HB_FUNC( HB_COMPILEBUF )
{
   int iResult, argc;
   char ** argv;
   BYTE * pBuffer;
   ULONG ulLen;

   hb_compGenArgList( 1, hb_pcount(), &argc, &argv );
   iResult = hb_compMain( argc, argv, &pBuffer, &ulLen );
   hb_xfree( argv );
   if( iResult == EXIT_SUCCESS && pBuffer )
      hb_retclen_buffer( ( char * ) pBuffer, ulLen );
}
