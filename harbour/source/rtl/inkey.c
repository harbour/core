/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Keyboard API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    HB_KEYPUT()
 *
 * Copyright 2002 Walter Negro <anegro@overnet.com.ar>
 *    hb_inkeySetLast()
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 *    HB_SETLASTKEY()
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz>
 *    HB_SETINKEYBEFOREBLOCK()
 *    HB_SETINKEYAFTERBLOCK()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbvm.h"

static PHB_ITEM s_inkeyBlockBefore = NULL;
static PHB_ITEM s_inkeyBlockAfter  = NULL;

HB_FUNC( INKEY )
{
   USHORT uiPCount = hb_pcount();
   PHB_ITEM pKey = NULL;
   int iKey;

   if( s_inkeyBlockBefore )
      hb_vmEvalBlock( s_inkeyBlockBefore );

   do
   {
      iKey = hb_inkey( uiPCount == 1 || ( uiPCount > 1 && ISNUM( 1 ) ),
                       hb_parnd( 1 ),
                       ISNUM( 2 ) ? hb_parni( 2 ) : hb_set.HB_SET_EVENTMASK );

      if( iKey == 0 || !s_inkeyBlockAfter )
         break;

      pKey = hb_itemPutNI( pKey, iKey );
      iKey = hb_itemGetNI( hb_vmEvalBlockV( s_inkeyBlockAfter, 1, pKey ) );
      hb_inkeySetLast( iKey );
   }
   while( iKey == 0 );

   if( pKey )
      hb_itemRelease( pKey );

   hb_retni( iKey );
}

/* temporary disabled */
#if 0

static BOOL s_fInit = FALSE;

static void hb_inkeyBlockFree( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_inkeyBlockBefore )
   {
      hb_itemRelease( s_inkeyBlockBefore );
      s_inkeyBlockBefore = NULL;
   }
   if( s_inkeyBlockAfter )
   {
      hb_itemRelease( s_inkeyBlockAfter );
      s_inkeyBlockAfter = NULL;
   }
}

static void hb_inkeySetDestructor( void )
{
   if( !s_fInit )
   {
      s_fInit = TRUE;
      hb_vmAtExit( hb_inkeyBlockFree, NULL );
   }
}

HB_FUNC( HB_SETINKEYBEFOREBLOCK )
{
   if( s_inkeyBlockBefore )
      hb_itemReturn( s_inkeyBlockBefore );

   if( hb_pcount() > 0 )
   {
      PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

      if( pBlock )
      {
         hb_inkeySetDestructor();
         pBlock = hb_itemNew( pBlock );
      }

      if( s_inkeyBlockBefore )
         hb_itemRelease( s_inkeyBlockBefore );
      s_inkeyBlockBefore = pBlock;
   }
}

HB_FUNC( HB_SETINKEYAFTERBLOCK )
{
   if( s_inkeyBlockAfter )
      hb_itemReturn( s_inkeyBlockAfter );

   if( hb_pcount() > 0 )
   {
      PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

      if( pBlock )
      {
         hb_inkeySetDestructor();
         pBlock = hb_itemNew( pBlock );
      }

      if( s_inkeyBlockAfter )
         hb_itemRelease( s_inkeyBlockAfter );
      s_inkeyBlockAfter = pBlock;
   }
}

#endif

HB_FUNC( __KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   hb_inkeyReset();

   if( ISCHAR( 1 ) )
      hb_inkeySetText( hb_parc( 1 ), hb_parclen( 1 ) );
}

HB_FUNC( HB_KEYPUT )
{
   if( ISNUM( 1 ) )
   {
      hb_inkeyPut( hb_parni( 1 ) );
   }
   else if( ISCHAR( 1 ) )
   {
      hb_inkeySetText( hb_parc( 1 ), hb_parclen( 1 ) );
   }
   else if( ISARRAY( 1 ) )
   {
      PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
      ULONG ulIndex;
      ULONG ulElements = hb_arrayLen( pArray );

      for( ulIndex = 1; ulIndex <= ulElements; ulIndex++ )
      {
         HB_TYPE type = hb_arrayGetType( pArray, ulIndex );

         if( type & HB_IT_NUMERIC )
         {
            hb_inkeyPut( hb_arrayGetNI( pArray, ulIndex ) );
         }
         else if( type & HB_IT_STRING )
         {
            hb_inkeySetText( ( const char * ) hb_arrayGetCPtr( pArray, ulIndex ), hb_arrayGetCLen( pArray, ulIndex ) );
         }
      }
   }
}

HB_FUNC( HB_KEYINS )
{
   if( ISNUM( 1 ) )
   {
      hb_inkeyIns( hb_parni( 1 ) );
   }
   else if( ISCHAR( 1 ) )
   {
      hb_inkeySetText( hb_parc( 1 ), hb_parclen( 1 ) );
   }
   else if( ISARRAY( 1 ) )
   {
      PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
      ULONG ulIndex;
      ULONG ulElements = hb_arrayLen( pArray );

      for( ulIndex = 1; ulIndex <= ulElements; ulIndex++ )
      {
         HB_TYPE type = hb_arrayGetType( pArray, ulIndex );

         if( type & HB_IT_NUMERIC )
         {
            hb_inkeyIns( hb_arrayGetNI( pArray, ulIndex ) );
         }
         else if( type & HB_IT_STRING )
         {
            hb_inkeySetText( ( const char * ) hb_arrayGetCPtr( pArray, ulIndex ), hb_arrayGetCLen( pArray, ulIndex ) );
         }
      }
   }
}

HB_FUNC( NEXTKEY )
{
   hb_retni( hb_inkeyNext( ISNUM( 1 ) ? hb_parni( 1 ) : hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( LASTKEY )
{
   hb_retni( hb_inkeyLast( ISNUM( 1 ) ? hb_parni( 1 ) : INKEY_ALL ) );
}

HB_FUNC( HB_SETLASTKEY )
{
   if( ISNUM( 1 ) )
      hb_retni( hb_inkeySetLast( hb_parni( 1 ) ) );
}
