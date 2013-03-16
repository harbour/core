/*
 * Harbour Project source code:
 * SetKey() and related functions
 *
 * Copyright 1999 April White <april@users.sourceforge.net>
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
   Either way you have to clean up the memory on exit. The best way to
   do this is to add a hb_setkeyInit() and hb_setkeyExit() function
   and call them from console.c Init/Exit functions.
 */

#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbstack.h"

typedef struct HB_SETKEY_
{
   int iKeyCode;
   PHB_ITEM pAction;
   PHB_ITEM pIsActive;
   struct HB_SETKEY_ * next;
} HB_SETKEY, * PHB_SETKEY;

typedef struct
{
   PHB_SETKEY sk_list;
} HB_SK_DATA, * PHB_SK_DATA;

static void hb_setkeyRelease( void * cargo )
{
   PHB_SETKEY sk_list = ( ( PHB_SK_DATA ) cargo )->sk_list;

   while( sk_list )
   {
      PHB_SETKEY sk_list_tmp;

      hb_itemRelease( sk_list->pAction );
      if( sk_list->pIsActive )
         hb_itemRelease( sk_list->pIsActive );
      sk_list_tmp = sk_list;
      sk_list = sk_list->next;
      hb_xfree( sk_list_tmp );
   }

   ( ( PHB_SK_DATA ) cargo )->sk_list = NULL;
}

static HB_TSD_NEW( s_skData, sizeof( HB_SK_DATA ), NULL, hb_setkeyRelease );

static PHB_SETKEY sk_findkey( int iKeyCode, PHB_SETKEY sk_list,
                              PHB_SETKEY * sk_list_end )
{
   PHB_SETKEY sk_list_tmp;

   *sk_list_end = NULL;
   for( sk_list_tmp = sk_list;
        sk_list_tmp && sk_list_tmp->iKeyCode != iKeyCode;
        sk_list_tmp = sk_list_tmp->next )
      *sk_list_end = sk_list_tmp;

   return sk_list_tmp;
}

static void sk_add( PHB_SETKEY * sk_list_ptr, HB_BOOL bReturn,
                    int iKeyCode, PHB_ITEM pAction, PHB_ITEM pIsActive )
{
   if( iKeyCode )
   {
      PHB_SETKEY sk_list_tmp, sk_list_end;

      if( pIsActive && ! HB_IS_BLOCK( pIsActive ) )
         pIsActive = NULL;
      if( pAction && ! HB_IS_BLOCK( pAction ) )
         pAction = NULL;

      sk_list_tmp = sk_findkey( iKeyCode, *sk_list_ptr, &sk_list_end );
      if( sk_list_tmp == NULL )
      {
         if( pAction )
         {
            sk_list_tmp = ( PHB_SETKEY ) hb_xgrab( sizeof( HB_SETKEY ) );
            sk_list_tmp->next = NULL;
            sk_list_tmp->iKeyCode = iKeyCode;
            sk_list_tmp->pAction = hb_itemNew( pAction );
            sk_list_tmp->pIsActive = pIsActive ? hb_itemNew( pIsActive ) : NULL;

            if( sk_list_end == NULL )
               *sk_list_ptr = sk_list_tmp;
            else
               sk_list_end->next = sk_list_tmp;
         }
      }
      else
      {
         /* Return the previous value */

         if( bReturn )
            hb_itemReturn( sk_list_tmp->pAction );

         /* Free the previous values */

         hb_itemRelease( sk_list_tmp->pAction );
         if( sk_list_tmp->pIsActive )
         {
            hb_itemRelease( sk_list_tmp->pIsActive );
         }
         /* Set the new values or free the entry */

         if( pAction )
         {
            sk_list_tmp->pAction = hb_itemNew( pAction );
            sk_list_tmp->pIsActive = pIsActive ? hb_itemNew( pIsActive ) : NULL;
         }
         else
         {
            /* if this is true, then the key found is the first key in the list */
            if( sk_list_end == NULL )
            {
               sk_list_tmp = *sk_list_ptr;
               *sk_list_ptr = sk_list_tmp->next;
               hb_xfree( sk_list_tmp );
            }
            else
            {
               sk_list_end->next = sk_list_tmp->next;
               hb_xfree( sk_list_tmp );
            }
         }
      }
   }
}

HB_FUNC( SETKEY )
{
   PHB_ITEM pKeyCode = hb_param( 1, HB_IT_NUMERIC );

   if( pKeyCode )
   {
      PHB_SK_DATA sk_data = ( PHB_SK_DATA ) hb_stackGetTSD( &s_skData );

      if( hb_pcount() == 1 )
      {
         /* Get a SETKEY value */
         PHB_SETKEY sk_list_tmp, sk_list_end;

         /* sk_list_end is not used in this context */
         sk_list_tmp = sk_findkey( hb_itemGetNI( pKeyCode ), sk_data->sk_list, &sk_list_end );

         if( sk_list_tmp )
            hb_itemReturn( sk_list_tmp->pAction );
      }
      else
      {
         /* Set a SETKEY value */
         sk_add( &sk_data->sk_list, HB_TRUE, hb_itemGetNI( pKeyCode ),
                 hb_param( 2, HB_IT_BLOCK ), NULL );
      }
   }
}

HB_FUNC( HB_SETKEY )
{
   PHB_ITEM pKeyCode = hb_param( 1, HB_IT_NUMERIC );

   if( pKeyCode )
   {
      PHB_SK_DATA sk_data = ( PHB_SK_DATA ) hb_stackGetTSD( &s_skData );

      if( hb_pcount() == 1 )
      {
         /* Get a SETKEY value */
         PHB_SETKEY sk_list_tmp, sk_list_end;

         /* sk_list_end is not used in this context */
         sk_list_tmp = sk_findkey( hb_itemGetNI( pKeyCode ), sk_data->sk_list, &sk_list_end );

         if( sk_list_tmp )
         {
            PHB_ITEM pIsActiveResults = sk_list_tmp->pIsActive ? hb_vmEvalBlockV( sk_list_tmp->pIsActive, 1, pKeyCode ) : NULL;

            if( pIsActiveResults == NULL || ! HB_IS_LOGICAL( pIsActiveResults ) || hb_itemGetL( pIsActiveResults ) )
               hb_itemReturn( sk_list_tmp->pAction );
         }
      }
      else
      {
         /* Set a SETKEY value */
         sk_add( &sk_data->sk_list, HB_TRUE, hb_itemGetNI( pKeyCode ),
                 hb_param( 2, HB_IT_BLOCK ), hb_param( 3, HB_IT_BLOCK ) );
      }
   }
}

/* Sets the same block for an array of keycodes */

HB_FUNC( HB_SETKEYARRAY )
{
   PHB_ITEM pKeyCodeArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pAction = hb_param( 2, HB_IT_BLOCK );

   if( pKeyCodeArray && pAction )
   {
      PHB_SK_DATA sk_data = ( PHB_SK_DATA ) hb_stackGetTSD( &s_skData );
      PHB_ITEM pIsActive = hb_param( 3, HB_IT_BLOCK );
      HB_SIZE nLen = hb_arrayLen( pKeyCodeArray );
      HB_SIZE nPos;

      for( nPos = 1; nPos <= nLen; nPos++ )
         sk_add( &sk_data->sk_list, HB_FALSE, hb_arrayGetNI( pKeyCodeArray, nPos ), pAction, pIsActive );
   }
}

HB_FUNC( HB_SETKEYGET )
{
   PHB_ITEM pKeyCode = hb_param( 1, HB_IT_NUMERIC );

   if( pKeyCode )
   {
      PHB_SK_DATA sk_data = ( PHB_SK_DATA ) hb_stackGetTSD( &s_skData );
      PHB_SETKEY sk_list_tmp, sk_list_end;

      /* sk_list_end is not used in this context */
      sk_list_tmp = sk_findkey( hb_itemGetNI( pKeyCode ), sk_data->sk_list, &sk_list_end );

      if( sk_list_tmp )
      {
         hb_itemReturn( sk_list_tmp->pAction );

         if( sk_list_tmp->pIsActive )
            hb_itemParamStore( 2, sk_list_tmp->pIsActive );
      }
   }
}

HB_FUNC( HB_SETKEYSAVE )
{
   PHB_SK_DATA sk_data = ( PHB_SK_DATA ) hb_stackGetTSD( &s_skData );
   PHB_ITEM pKeys, pKeyElements, pParam;
   PHB_SETKEY sk_list_tmp;
   HB_SIZE nItemCount, nItem;

   /* build an multi-dimensional array from existing hot-keys, and return it */

   /* count the number of items in the list */
   for( nItemCount = 0, sk_list_tmp = sk_data->sk_list;
        sk_list_tmp;
        nItemCount++, sk_list_tmp = sk_list_tmp->next )
      ;

   pKeys = hb_itemArrayNew( nItemCount );
   pKeyElements = hb_itemNew( NULL );

   for( nItem = 1, sk_list_tmp = sk_data->sk_list;
        nItem <= nItemCount;
        nItem++, sk_list_tmp = sk_list_tmp->next )
   {
      hb_arrayNew( pKeyElements, 3 );
      hb_arraySetNI( pKeyElements, 1, sk_list_tmp->iKeyCode );
      hb_arraySet( pKeyElements, 2, sk_list_tmp->pAction );
      if( sk_list_tmp->pIsActive )
         hb_arraySet( pKeyElements, 3, sk_list_tmp->pIsActive );
      hb_arraySetForward( pKeys, nItem, pKeyElements );
   }
   hb_itemRelease( pKeyElements );
   hb_itemReturnRelease( pKeys );

   pParam = hb_param( 1, HB_IT_ANY );
   if( pParam )
   {
      hb_setkeyRelease( sk_data ); /* destroy the internal list */

      if( HB_IS_ARRAY( pParam ) )
      {
         nItemCount = hb_arrayLen( pParam );

         for( nItem = 1; nItem <= nItemCount; nItem++ )
         {
            PHB_ITEM itmKeyElements = hb_arrayGetItemPtr( pParam, nItem );

            sk_add( &sk_data->sk_list, HB_FALSE,
                    hb_arrayGetNI( itmKeyElements, 1 ),
                    hb_arrayGetItemPtr( itmKeyElements, 2 ),
                    hb_arrayGetItemPtr( itmKeyElements, 3 ) );
         }
      }
   }
}

HB_FUNC( HB_SETKEYCHECK )
{
   PHB_ITEM pKeyCode = hb_param( 1, HB_IT_NUMERIC );
   HB_BOOL bIsKeySet = HB_FALSE;

   if( pKeyCode )
   {
      PHB_SK_DATA sk_data = ( PHB_SK_DATA ) hb_stackGetTSD( &s_skData );
      PHB_SETKEY sk_list_tmp, sk_list_end;

      /* sk_list_end is not used in this context */
      sk_list_tmp = sk_findkey( hb_itemGetNI( pKeyCode ), sk_data->sk_list, &sk_list_end );

      if( sk_list_tmp )
      {
         PHB_ITEM pIsActiveResults = sk_list_tmp->pIsActive ? hb_vmEvalBlockV( sk_list_tmp->pIsActive, 1, pKeyCode ) : NULL;

         if( pIsActiveResults == NULL || ! HB_IS_LOGICAL( pIsActiveResults ) || hb_itemGetL( pIsActiveResults ) )
         {
            bIsKeySet = HB_TRUE;

            switch( hb_pcount() )
            {
               case 1:  hb_vmEvalBlockV( sk_list_tmp->pAction, 1, pKeyCode ); break;
               case 2:  hb_vmEvalBlockV( sk_list_tmp->pAction, 2, hb_param( 2, HB_IT_ANY ), pKeyCode ); break;
               case 3:  hb_vmEvalBlockV( sk_list_tmp->pAction, 3, hb_param( 2, HB_IT_ANY ), hb_param( 3, HB_IT_ANY ), pKeyCode ); break;
               default: hb_vmEvalBlockV( sk_list_tmp->pAction, 4, hb_param( 2, HB_IT_ANY ), hb_param( 3, HB_IT_ANY ), hb_param( 4, HB_IT_ANY ), pKeyCode ); break;
            }
         }
      }
   }

   hb_retl( bIsKeySet );
}
