/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Language API
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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
#include "hbapilng.h"

/* Always link in the default language */
HB_LANG_REQUEST( HB_LANG_DEFAULT );

/* NOTE: This is the maximum number of registered languages, later this can be
         made dynamic. */
#define HB_LANG_MAX_ 64

#define HB_LANG_ITEM_ID_ID         0
#define HB_LANG_ITEM_ID_NAME       1
#define HB_LANG_ITEM_ID_NAMENAT    2
#define HB_LANG_ITEM_ID_RFCID      3
#define HB_LANG_ITEM_ID_CODEPAGE   4

static PHB_LANG s_langList[ HB_LANG_MAX_ ];
static PHB_LANG s_lang = NULL;

static int hb_langFindPos( char * pszID )
{
   int iPos;

   if( pszID != NULL )
   {
      for( iPos = 0; iPos < HB_LANG_MAX_; iPos++ )
      {
         if( s_langList[ iPos ] != NULL && strcmp( ( char * ) s_langList[ iPos ]->pItemList[ HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID ], pszID ) == 0 )
            return iPos;
      }
   }

   return -1;
}

BOOL hb_langRegister( PHB_LANG lang )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_langRegister(%p)", lang));

   if( lang )
   {
      int iPos = hb_langFindPos( ( char * ) lang->pItemList[ HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID ] );

      if( iPos == -1 )
      {
         for( iPos = 0; iPos < HB_LANG_MAX_; iPos++ )
         {
            if( s_langList[ iPos ] == NULL )
            {
               s_langList[ iPos ] = lang;
               return TRUE;
            }
         }
      }
      else
      {
         s_langList[ iPos ] = lang;
         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_langDeRegister( char * pszID )
{
   int iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_langDeRegister(%s)", pszID));

   iPos = hb_langFindPos( pszID );

   if( iPos != -1 )
   {
      s_langList[ iPos ] = NULL;
      return TRUE;
   }
   else
      return FALSE;
}

PHB_LANG hb_langFind( char * pszID )
{
   int iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_langFind(%s)", pszID));

   iPos = hb_langFindPos( pszID );

   return ( iPos != -1 ) ? s_langList[ iPos ] : NULL;
}

PHB_LANG hb_langSelect( PHB_LANG lang )
{
   PHB_LANG langOld = s_lang;

   HB_TRACE(HB_TR_DEBUG, ("hb_langSelect(%p)", lang));

   if( lang )
      s_lang = lang;

   return langOld;
}

char * hb_langSelectID( char * pszID )
{
   char * pszIDOld = hb_langID();

   HB_TRACE(HB_TR_DEBUG, ("hb_langSelectID(%s)", pszID));

   hb_langSelect( hb_langFind( pszID ) );

   return pszIDOld;
}

char * hb_langDGetItem( int iIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_langDGetItem(%i)", iIndex));

   if( s_lang && iIndex >= 0 && iIndex < HB_LANG_ITEM_MAX_ )
      return (char *) s_lang->pItemList[ iIndex ];
   else
      return NULL;
}

char * hb_langID( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_langID()"));

   if( s_lang )
      return ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID );
   else
      return NULL;
}

/* NOTE: Caller must free the pointer. */

char * hb_langName( void )
{
   char * pszName = ( char * ) hb_xgrab( 128 );

   if( s_lang )
      sprintf( pszName, "Harbour Language: %s %s (%s)",
         ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID ),
         ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_NAME ),
         ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_NAMENAT ) );
   else
      strcpy( pszName, "Harbour Language: (not installed)" );

   return pszName;
}

/* Compatibility interface */

char * hb_langDGetErrorDesc( ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_langDGetErrorDesc(%lu)", ulIndex));

   return ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRDESC + ulIndex );
}

/* Harbour interface */

HB_FUNC( HB_LANGSELECT )
{
   hb_retc( hb_langID() );

   hb_langSelectID( hb_parc( 1 ) );
}

HB_FUNC( HB_LANGNAME )
{
   char * pszName = hb_langName();
   hb_retc( pszName );
   hb_xfree( pszName );
}

HB_FUNC( HB_LANGERRMSG )
{
   hb_retc( hb_langDGetErrorDesc( hb_parnl( 1 ) ) );
}

HB_FUNC( HB_LANGMESSAGE )
{
   hb_retc( hb_langDGetItem( hb_parnl( 1 ) ) );
}
