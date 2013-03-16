/*
 * Harbour Project source code:
 * Calling function from dynamic library (hb_DynCall())
 *
 * Copyright 2009-2010 Viktor Szakats (harbour syenar.net)
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbdyn.ch"

HB_FUNC( HB_DYNCALL )
{
   PHB_ITEM pParam = hb_param( 1, HB_IT_POINTER | HB_IT_ARRAY );
   int * piArgFlags = NULL;
   int iFuncFlags = HB_DYN_CALLCONV_CDECL;

   PHB_ITEM pLibraryHandle = NULL;
   HB_BOOL bFreeLibrary = HB_FALSE;
   void * pFunctionPtr = NULL;

   if( pParam )
   {
      if( HB_IS_ARRAY( pParam ) )
      {
         HB_SIZE nLen = hb_arrayLen( pParam );

         if( nLen >= 1 )
         {
            PHB_ITEM pFunction = hb_arrayGetItemPtr( pParam, 1 );
            HB_SIZE nBasePos = 2;

            if( HB_IS_STRING( pFunction ) && nLen >= nBasePos )
            {
               PHB_ITEM pLibrary = hb_arrayGetItemPtr( pParam, nBasePos );

               if( HB_IS_STRING( pLibrary ) )
               {
                  pLibraryHandle = hb_libLoad( pLibrary, NULL );
                  if( pLibraryHandle )
                     bFreeLibrary = HB_TRUE;
               }
               else if( hb_libHandle( pLibrary ) )
                  pLibraryHandle = pLibrary;

               if( pLibraryHandle )
                  pFunctionPtr = hb_libSymAddr( pLibraryHandle, hb_itemGetCPtr( pFunction ) );

               ++nBasePos;
            }

            /* Function flags */
            if( nBasePos <= nLen )
               iFuncFlags = hb_arrayGetNI( pParam, nBasePos );

            ++nBasePos;

            /* Argument flags */
            if( nBasePos <= nLen )
            {
               HB_SIZE nPos;
               HB_SIZE nArgCount = hb_pcount() - 1;

               piArgFlags = ( int * ) hb_xgrab( sizeof( int ) * nArgCount );

               for( nPos = 0; nPos < nArgCount; ++nPos )
                  piArgFlags[ nPos ] = ( ( nPos + nBasePos ) <= nLen && HB_IS_NUMERIC( hb_arrayGetItemPtr( pParam, nPos + nBasePos ) ) ) ? hb_arrayGetNI( pParam, nPos + nBasePos ) : HB_DYN_CTYPE_DEFAULT;
            }
         }
      }
   }

   hb_dynCall( iFuncFlags, pFunctionPtr, hb_pcount(), 2, piArgFlags );

   if( piArgFlags )
      hb_xfree( piArgFlags );

   if( bFreeLibrary )
      hb_libFree( pLibraryHandle );
}
