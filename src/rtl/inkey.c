/*
 * Harbour Project source code:
 * The Keyboard API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2012 Viktor Szakats (vszakats.net/harbour)
 *    hb_keyPut()
 *
 * Copyright 2003-2012 Przemyslaw Czerpak <druzus@acn.waw.pl>
 *    hb_keySetLast(), hb_keyChar(), hb_keyStd()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapigt.h"
#include "hbgtcore.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbset.h"
#include "hbstack.h"
#include "hbvm.h"

static void hb_inkeySetTextKeys( const char * pszText, HB_SIZE nSize, HB_BOOL fInsert )
{
   PHB_CODEPAGE cdp = hb_vmCDP();
   HB_SIZE nIndex = 0;
   HB_WCHAR wc;

   if( fInsert )
   {
      HB_WCHAR buffer[ 32 ], * keys;
      HB_SIZE n = 0;

      keys = nSize <= HB_SIZEOFARRAY( buffer ) ? buffer :
                        ( HB_WCHAR * ) hb_xgrab( nSize * sizeof( HB_WCHAR ) );
      while( HB_CDPCHAR_GET( cdp, pszText, nSize, &nIndex, &wc ) )
         keys[ n++ ] = wc;

      while( n-- )
      {
         int iKey = keys[ n ] >= 128 ? HB_INKEY_NEW_UNICODE( keys[ n ] ) : keys[ n ];
         hb_inkeyIns( iKey );
      }
      if( nSize > HB_SIZEOFARRAY( buffer ) )
         hb_xfree( keys );
   }
   else
   {
      while( HB_CDPCHAR_GET( cdp, pszText, nSize, &nIndex, &wc ) )
      {
         int iKey = wc >= 128 ? HB_INKEY_NEW_UNICODE( wc ) : wc;
         hb_inkeyPut( iKey );
      }
   }
}

HB_FUNC( INKEY )
{
   int iPCount = hb_pcount();

   hb_retni( hb_inkey( iPCount == 1 || ( iPCount > 1 && HB_ISNUM( 1 ) ),
                       hb_parnd( 1 ), hb_parnidef( 2, hb_setGetEventMask() ) ) );
}

HB_FUNC( __KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   hb_inkeyReset();

   if( HB_ISCHAR( 1 ) )
      hb_inkeySetText( hb_parc( 1 ), hb_parclen( 1 ) );
}

HB_FUNC( HB_KEYCLEAR )
{
   hb_inkeyReset();
}

HB_FUNC( HB_KEYPUT )
{
   if( HB_ISNUM( 1 ) )
   {
      hb_inkeyPut( hb_parni( 1 ) );
   }
   else if( HB_ISCHAR( 1 ) )
   {
      hb_inkeySetTextKeys( hb_parc( 1 ), hb_parclen( 1 ), HB_FALSE );
   }
   else if( HB_ISARRAY( 1 ) )
   {
      PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
      HB_SIZE nIndex;
      HB_SIZE nElements = hb_arrayLen( pArray );

      for( nIndex = 1; nIndex <= nElements; ++nIndex )
      {
         HB_TYPE type = hb_arrayGetType( pArray, nIndex );

         if( type & HB_IT_NUMERIC )
         {
            hb_inkeyPut( hb_arrayGetNI( pArray, nIndex ) );
         }
         else if( type & HB_IT_STRING )
         {
            hb_inkeySetTextKeys( hb_arrayGetCPtr( pArray, nIndex ),
                                 hb_arrayGetCLen( pArray, nIndex ), HB_FALSE );
         }
      }
   }
}

HB_FUNC( HB_KEYINS )
{
   if( HB_ISNUM( 1 ) )
   {
      hb_inkeyIns( hb_parni( 1 ) );
   }
   else if( HB_ISCHAR( 1 ) )
   {
      hb_inkeySetTextKeys( hb_parc( 1 ), hb_parclen( 1 ), HB_TRUE );
   }
   else if( HB_ISARRAY( 1 ) )
   {
      PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
      HB_SIZE nIndex;
      HB_SIZE nElements = hb_arrayLen( pArray );

      for( nIndex = 1; nIndex <= nElements; ++nIndex )
      {
         HB_TYPE type = hb_arrayGetType( pArray, nIndex );

         if( type & HB_IT_NUMERIC )
         {
            hb_inkeyIns( hb_arrayGetNI( pArray, nIndex ) );
         }
         else if( type & HB_IT_STRING )
         {
            hb_inkeySetTextKeys( hb_arrayGetCPtr( pArray, nIndex ),
                                 hb_arrayGetCLen( pArray, nIndex ), HB_TRUE );
         }
      }
   }
}

HB_FUNC( HB_KEYNEXT )
{
   hb_retni( hb_inkeyNext( HB_ISNUM( 1 ) ? hb_parni( 1 ) : hb_setGetEventMask() ) );
}

HB_FUNC( NEXTKEY )
{
   hb_retni( hb_inkeyNext( hb_setGetEventMask() ) );
}

HB_FUNC( HB_KEYLAST )
{
   hb_retni( hb_inkeyLast( HB_ISNUM( 1 ) ? hb_parni( 1 ) : hb_setGetEventMask() ) );
}

HB_FUNC( LASTKEY )
{
   hb_retni( hb_inkeyLast( HB_INKEY_ALL ) );
}

HB_FUNC( HB_KEYSETLAST )
{
   if( HB_ISNUM( 1 ) )
      hb_retni( hb_inkeySetLast( hb_parni( 1 ) ) );
}

#if defined( HB_LEGACY_LEVEL5 )

HB_FUNC_TRANSLATE( HB_SETLASTKEY, HB_KEYSETLAST )

#endif

HB_FUNC( HB_KEYCODE )
{
   const char * szValue = hb_parc( 1 );
   int iKey;

   if( szValue )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE nIndex = 0;
      HB_WCHAR wc;

      if( HB_CDPCHAR_GET( cdp, szValue, hb_parclen( 1 ), &nIndex, &wc ) )
         iKey = wc >= 128 ? HB_INKEY_NEW_UNICODE( wc ) : wc;
      else
         iKey = 0;
   }
   else
      iKey = 0;

   hb_retni( iKey );
}

HB_FUNC( HB_KEYCHAR )
{
   char szKeyChr[ HB_MAX_CHAR_LEN ];
   HB_SIZE nLen;

   nLen = hb_inkeyKeyString( hb_parni( 1 ), szKeyChr, sizeof( szKeyChr ) );
   hb_retclen( szKeyChr, nLen );
}

HB_FUNC( HB_KEYSTD )
{
   hb_retni( hb_inkeyKeyStd( hb_parni( 1 ) ) );
}

HB_FUNC( HB_KEYMOD )
{
   hb_retni( hb_inkeyKeyMod( hb_parni( 1 ) ) );
}

HB_FUNC( HB_KEYVAL )
{
   hb_retni( hb_inkeyKeyVal( hb_parni( 1 ) ) );
}
