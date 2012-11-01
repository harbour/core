/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * C Structure Support.
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xharbour.org
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

#include "hbvmint.h" /* TOFIX: clean the code to not access any internal HVM structures */
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbctypes.ch"

static PHB_ITEM hb_itemPutCRaw( PHB_ITEM pItem, const char * szText, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutCRaw(%p, %s, %" HB_PFS "u)", pItem, szText, nLen ) );

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   if( nLen == 0 )
   {
      if( szText )
         hb_xfree( ( void * ) szText );
      szText = "";
   }
   pItem->type = HB_IT_STRING;
   pItem->item.asString.length    = nLen;
   pItem->item.asString.value     = ( char * ) szText;
   pItem->item.asString.allocated = nLen;

   return pItem;
}

#undef hb_itemPutCRawStatic
static PHB_ITEM hb_itemPutCRawStatic( PHB_ITEM pItem, const char * szText, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutCRawStatic(%p, %s, %" HB_PFS "u)", pItem, szText, nLen ) );

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( NULL );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.length    = nLen;
   pItem->item.asString.value     = ( char * ) szText;

   return pItem;
}

#if 0

#undef hb_retclenAdoptRaw
void hb_retclenAdoptRaw( const char * szText, HB_SIZE nLen )
{
   hb_itemPutCRaw( hb_stackReturnItem(), szText, nLen );
}

#undef hb_retclenStatic
void hb_retclenStatic( const char * szText, HB_SIZE nLen )
{
   hb_itemPutCRawStatic( hb_stackReturnItem(), szText, nLen );
}

#endif

static HB_UINT SizeOfCStructure( PHB_ITEM aDef, HB_UINT uiAlign )
{
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   HB_SIZE       nLen     = pBaseDef->nLen;
   HB_SIZE       nIndex;
   HB_UINT       uiSize = 0, uiMemberSize;
   HB_BYTE       cShift;
   HB_UINT       uiPad;

   for( nIndex = 0; nIndex < nLen; nIndex++ )
   {
      if( ( pBaseDef->pItems + nIndex )->type != HB_IT_INTEGER )
      {
         hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
         return 0;
      }

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:          /* char */
         case CTYPE_UNSIGNED_CHAR: /* unsigned char */
            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR:          /* short */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short */
            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT:          /* int */
         case CTYPE_UNSIGNED_INT: /* unsigned int */
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG:          /* long */
         case CTYPE_UNSIGNED_LONG: /* unsigned long */
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT:  /* float */
            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE:  /* double */
            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            uiMemberSize = sizeof( void * );
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof( void * );
            }
            else if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pID        = hb_itemPutNI( NULL, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               hb_itemRelease( pID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "SizeOf", 0 );
                  uiMemberSize = ( HB_UINT ) hb_parns( -1 );
                  hb_itemRelease( pStructure );
               }
               else
               {
                  hb_itemRelease( pStructure );
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
                  return 0;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
               return 0;
            }
      }

      if( uiSize )
      {
         uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( HB_BYTE ) ( uiSize % uiPad ) ) > 0 )
         {
            uiSize += ( uiPad - cShift );
         }
      }

      uiSize += uiMemberSize;

      /* printf( "#%" HB_PFS "u Size: %u Align: %u Pad: %u Shift %i Size: %u\n", nIndex, uiMemberSize, uiAlign, uiPad, cShift, uiSize ); */

   }

   if( ( cShift = ( HB_BYTE ) ( uiSize % uiAlign ) ) > 0 )
   {
      uiSize += ( uiAlign - cShift );
   }

   /* printf( "#%" HB_PFS "u Size: %u Align: %u Pad: %u Shift %i Size: %u\n", nIndex, uiMemberSize, uiAlign, uiPad, cShift, uiSize ); */

   return uiSize;
}

HB_FUNC( HB_SIZEOFCSTRUCTURE )
{
   PHB_ITEM aDef   = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pAlign = hb_param( 2, HB_IT_INTEGER );
   HB_UINT  uiAlign;

   if( aDef )
   {
      if( pAlign )
      {
         uiAlign = ( HB_BYTE ) pAlign->item.asInteger.value;
      }
      else
      {
         uiAlign = 8;
      }

      hb_retni( SizeOfCStructure( aDef, uiAlign ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

static HB_BYTE * ArrayToStructure( PHB_ITEM aVar, PHB_ITEM aDef, HB_UINT uiAlign, HB_UINT * puiSize )
{
   PHB_BASEARRAY pBaseVar = aVar->item.asArray.value;
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   HB_SIZE       nLen     = pBaseDef->nLen;
   HB_SIZE       nIndex;
   HB_BYTE *     Buffer;
   HB_UINT       uiOffset = 0, uiMemberSize;
   HB_BYTE       cShift;

   *puiSize = SizeOfCStructure( aDef, uiAlign );

   /* printf( "Size: %i\n", *puiSize ); */

   Buffer = ( HB_BYTE * ) hb_xgrab( *puiSize + 1 );

   for( nIndex = 0; nIndex < nLen; nIndex++ )
   {
      /* printf( "#: %i\n", nIndex ); */

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:          /* char */
         case CTYPE_UNSIGNED_CHAR: /* unsigned char */
            if( ( pBaseVar->pItems + nIndex )->type && ! HB_IS_NUMERIC( pBaseVar->pItems + nIndex  ) )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_STRING &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_POINTER &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR:          /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_POINTER &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT:          /* int */
         case CTYPE_UNSIGNED_INT: /* unsigned int */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_POINTER &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG:          /* long */
         case CTYPE_UNSIGNED_LONG: /* unsigned long */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_POINTER &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT:  /* float */
            if( ( pBaseVar->pItems + nIndex )->type && ( pBaseVar->pItems + nIndex )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_DOUBLE &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_POINTER &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE:  /* double */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_DOUBLE &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_POINTER &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_POINTER &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_LONG &&
                ( pBaseVar->pItems + nIndex )->type != HB_IT_STRING )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( void * );
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof( void * );
            }
            else if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pID        = hb_itemPutNI( NULL, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               hb_itemRelease( pID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "SizeOf", 0 );
                  uiMemberSize = ( HB_UINT ) hb_parns( -1 );
                  hb_itemRelease( pStructure );
               }
               else
               {
                  hb_itemRelease( pStructure );
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
                  return NULL;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
      }

      if( uiOffset )
      {
         HB_UINT uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( HB_BYTE ) ( uiOffset % uiPad ) ) > 0 )
         {
            uiOffset += ( uiPad - cShift );
         }
      }

      /* printf( "* Size: %i Offset: %i\n", uiMemberSize, uiOffset ); */

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:  /* char */
            if( ( pBaseVar->pItems + nIndex )->type )
            {
               *( ( char * ) ( Buffer + uiOffset ) ) = ( char ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else
            {
               *( ( char * ) ( Buffer + uiOffset ) ) = 0;
            }
            break;

         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            if( ( pBaseVar->pItems + nIndex )->type )
            {
               *( ( HB_BYTE * ) ( Buffer + uiOffset ) ) = ( HB_BYTE ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else
            {
               *( ( HB_BYTE * ) ( Buffer + uiOffset ) ) = 0;
            }
            break;

         case CTYPE_CHAR_PTR:  /* char * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_STRING:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + nIndex )->item.asString.value;
                  break;

               case HB_IT_POINTER:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( char * ) ( pBaseVar->pItems + nIndex )->item.asPointer.value;
                  break;
#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( char * ) ( HB_PTRDIFF ) ( pBaseVar->pItems + nIndex )->item.asInteger.value;
                  break;
#endif
               case HB_IT_LONG:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( char * ) ( HB_PTRDIFF ) ( pBaseVar->pItems + nIndex )->item.asLong.value;
                  break;

               default:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_CHAR_PTR:  /* unsigned char * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_STRING:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = ( HB_BYTE * ) ( ( pBaseVar->pItems + nIndex )->item.asString.value );
                  break;

               case HB_IT_POINTER:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = ( HB_BYTE * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = ( HB_BYTE * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = ( HB_BYTE * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_SHORT:  /* short */
            if( ( pBaseVar->pItems + nIndex )->type == HB_IT_INTEGER )
            {
               *( ( short * ) ( Buffer + uiOffset ) ) = ( short ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_LONG )
            {
               *( ( short * ) ( Buffer + uiOffset ) ) = ( short ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_DOUBLE )
            {
               *( ( short * ) ( Buffer + uiOffset ) ) = ( short ) ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_NIL )
            {
               *( ( short * ) ( Buffer + uiOffset ) ) = 0;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_SHORT:  /* unsigned short */
            if( ( pBaseVar->pItems + nIndex )->type == HB_IT_INTEGER )
            {
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = ( unsigned short ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_LONG )
            {
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = ( unsigned short ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_DOUBLE )
            {
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = ( unsigned short ) ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_NIL )
            {
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = 0;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_SHORT_PTR:  /* short * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_SHORT_PTR:  /* unsigned short * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_INT:  /* int */
            if( ( pBaseVar->pItems + nIndex )->type == HB_IT_INTEGER )
            {
               *( ( int * ) ( Buffer + uiOffset ) ) = ( int ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_LONG )
            {
               *( ( int * ) ( Buffer + uiOffset ) ) = ( int ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_DOUBLE )
            {
               *( ( int * ) ( Buffer + uiOffset ) ) = ( int ) ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_NIL )
            {
               *( ( int * ) ( Buffer + uiOffset ) ) = 0;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_INT:  /* unsigned int */
            if( ( pBaseVar->pItems + nIndex )->type == HB_IT_INTEGER )
            {
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = ( unsigned int ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_LONG )
            {
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = ( unsigned int ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_DOUBLE )
            {
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = ( unsigned int ) ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_NIL )
            {
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = 0;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            break;

         case CTYPE_INT_PTR:  /* int * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_INT_PTR:  /* unsigned int * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_LONG:  /* long */
            if( ( pBaseVar->pItems + nIndex )->type == HB_IT_INTEGER )
            {
               *( ( long * ) ( Buffer + uiOffset ) ) = ( long ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_LONG )
            {
               *( ( long * ) ( Buffer + uiOffset ) ) = ( long ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_DOUBLE )
            {
               *( ( long * ) ( Buffer + uiOffset ) ) = ( long ) ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_NIL )
            {
               *( ( long * ) ( Buffer + uiOffset ) ) = 0;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            if( ( pBaseVar->pItems + nIndex )->type == HB_IT_INTEGER )
            {
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = ( unsigned long ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_LONG )
            {
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = ( unsigned long ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_DOUBLE )
            {
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = ( unsigned long ) ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            }
            else if( ( pBaseVar->pItems + nIndex )->type == HB_IT_NIL )
            {
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = 0;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_LONG_PTR:  /* long * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_LONG_PTR:  /* unsigned long * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_FLOAT:  /* float */
            if( ( pBaseVar->pItems + nIndex )->type )
            {
               *( ( float * ) ( Buffer + uiOffset ) ) = ( float ) ( pBaseVar->pItems + nIndex )->item.asDouble.value;
            }
            else
            {
               *( ( float * ) ( Buffer + uiOffset ) ) = 0;
            }
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               /* Is this correct??? IMHO It's a bug */
               case HB_IT_DOUBLE:
                  **( ( float ** ) ( Buffer + uiOffset ) ) = ( float ) ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
                  break;

               default:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_DOUBLE:  /* double */
            if( ( pBaseVar->pItems + nIndex )->type )
            {
               *( ( double * ) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + nIndex )->item.asDouble.value;
            }
            else
            {
               *( ( double * ) ( Buffer + uiOffset ) ) = 0;
            }
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               /* Is this correct??? IMHO It's a bug */
               case HB_IT_DOUBLE:
                  **( ( double ** ) ( Buffer + uiOffset ) ) = ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
                  break;

               default:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_VOID_PTR:  /* void * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRDIFF ) ( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = pBaseVar->pItems + nIndex;

               if( HB_IS_LONG( pStructure ) )
               {
                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                  {
                     *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRDIFF ) pStructure->item.asLong.value;
                  }
                  else
                  {
                     memcpy( ( void * ) ( Buffer + uiOffset ), ( void * ) ( HB_PTRDIFF ) pStructure->item.asLong.value, uiMemberSize );
                  }
               }
#if UINT_MAX == ULONG_MAX
               else if( HB_IS_INTEGER( pStructure ) )
               {
                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                  {
                     *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRDIFF ) pStructure->item.asInteger.value;
                  }
                  else
                  {
                     memcpy( ( void * ) ( Buffer + uiOffset ), ( void * ) ( HB_PTRDIFF ) pStructure->item.asInteger.value, uiMemberSize );
                  }
               }
#endif
               else if( HB_IS_NIL( pStructure ) )
               {
                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                  {
                     *( ( void ** ) ( Buffer + uiOffset ) ) = NULL;
                  }
                  else
                  {
                     /* TraceLog( NULL,"ArrayToStructure() - Empty Inplace\n" ); */
                     memset( ( void * ) ( Buffer + uiOffset ), 0, uiMemberSize );
                  }
               }
               else if( strncmp( hb_objGetClsName( pStructure ), "C Structure", 11 ) == 0 )
               {
                  PHB_BASEARRAY pBaseStructure  = pStructure->item.asArray.value;
                  PHB_ITEM      pInternalBuffer = pBaseStructure->pItems + pBaseStructure->nLen - 1;

                  hb_objSendMsg( pStructure, "VALUE", 0 );

                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                  {
                     *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) pInternalBuffer->item.asString.value;
                  }
                  else
                  {
                     memcpy( ( void * ) ( Buffer + uiOffset ), ( void * ) pInternalBuffer->item.asString.value, uiMemberSize );
                  }
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
            }
      }

      /* printf( "Wrote %i bytes at Offset %i\n", uiMemberSize, uiOffset ); */

      uiOffset += uiMemberSize;
   }

   return Buffer;
}

HB_FUNC( HB_ARRAYTOSTRUCTURE )
{
   PHB_ITEM aVar   = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM aDef   = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pAlign = hb_param( 3, HB_IT_INTEGER );

   if( aVar && aDef )
   {
      HB_UINT   uiSize;
      HB_UINT   uiAlign;
      HB_BYTE * Buffer;

      if( pAlign )
      {
         uiAlign = ( HB_BYTE ) pAlign->item.asInteger.value;
      }
      else
      {
         uiAlign = 8;
      }

      Buffer = ArrayToStructure( aVar, aDef, uiAlign, &uiSize );
      hb_retclen_buffer( ( char * ) Buffer, uiSize );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

static PHB_ITEM StructureToArray( HB_BYTE * Buffer, HB_SIZE nBufferLen, PHB_ITEM aDef, HB_UINT uiAlign, HB_BOOL bAdoptNested, PHB_ITEM pRet )
{
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   HB_SIZE       nLen     = pBaseDef->nLen;
   HB_SIZE       nIndex;
   HB_UINT       uiOffset, uiMemberSize;
   HB_BYTE       cShift;
   /* PHB_ITEM pRet = hb_itemNew( NULL ); */
   PHB_BASEARRAY pBaseVar;

   /* TraceLog( NULL, "StructureToArray(%p, %p, %u, %i) ->%u\n", Buffer, aDef, uiAlign, bAdoptNested, nLen ); */

   /* hb_arrayNew( pRet, nLen ); */
   pBaseVar = pRet->item.asArray.value;

   uiOffset = 0;
   for( nIndex = 0; nIndex < nLen; nIndex++ )
   {
      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:          /* char */
         case CTYPE_UNSIGNED_CHAR: /* unsigned char */
            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR:          /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short * */
            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT:          /* int */
         case CTYPE_UNSIGNED_INT: /* unsigned int */
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG:          /* long */
         case CTYPE_UNSIGNED_LONG: /* unsigned long */
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT:  /* float */
            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE:  /* double */
            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            uiMemberSize = sizeof( void * );
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof( void * );
            }
            else if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pID        = hb_itemPutNI( NULL, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               hb_itemRelease( pID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "SizeOf", 0 );
                  uiMemberSize = ( HB_UINT ) hb_parns( -1 );
                  hb_itemRelease( pStructure );
               }
               else
               {
                  hb_itemRelease( pStructure );
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 1, hb_paramError( 1 ) );
                  return pRet;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return pRet;
            }
      }

      if( uiOffset )
      {
         HB_UINT uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( HB_BYTE ) ( uiOffset % uiPad ) ) > 0 )
         {
            uiOffset += ( uiPad - cShift );
         }

         /* TraceLog( NULL, "* Size: %i Offset: %i Pad: %i\n", uiMemberSize, uiOffset, uiPad ); */
      }
      else
      {
         /* TraceLog( NULL, "* Size: %i Offset: %i\n", uiMemberSize, uiOffset ); */
      }

      if( ( uiOffset + uiMemberSize ) > nBufferLen )
      {
         break;
      }

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:  /* char */
            hb_itemPutNI( pBaseVar->pItems + nIndex, ( int ) *( ( char * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            hb_itemPutNI( pBaseVar->pItems + nIndex, ( int ) *( ( HB_BYTE * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_CHAR_PTR:  /* char * */
            if( HB_IS_STRING( pBaseVar->pItems + nIndex ) && ( pBaseVar->pItems + nIndex )->item.asString.value == *( ( char ** ) ( Buffer + uiOffset ) ) )
            {
               /* TraceLog( NULL, "IDENTICAL: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) ); */
            }
            else if( ! bAdoptNested )
            {
               /* TraceLog( NULL, "Static: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) ); */
               hb_itemPutCConst( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            else
            {
               /* TraceLog( NULL, "Adopt: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) ); */
               hb_itemPutC( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            break;

         case CTYPE_UNSIGNED_CHAR_PTR:  /* unsigned char * */
            if( HB_IS_STRING( pBaseVar->pItems + nIndex ) && ( pBaseVar->pItems + nIndex )->item.asString.value == *( ( char ** ) ( Buffer + uiOffset ) ) )
            {
               /* TraceLog( NULL, "IDENTICAL: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) ); */
            }
            else if( ! bAdoptNested )
            {
               /* TraceLog( NULL, "Static: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) ); */
               hb_itemPutCConst( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            else
            {
               /* TraceLog( NULL, "Adopt: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) ); */
               hb_itemPutC( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            break;

         case CTYPE_SHORT:  /* short */
            hb_itemPutNI( pBaseVar->pItems + nIndex, *( ( short * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_SHORT:  /* unsigned short */
            hb_itemPutNI( pBaseVar->pItems + nIndex, ( short ) *( ( unsigned short * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_SHORT_PTR:          /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_INT:  /* int */
            hb_itemPutNI( pBaseVar->pItems + nIndex, *( ( int * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_INT:  /* unsigned int */
            hb_itemPutNI( pBaseVar->pItems + nIndex, ( int ) *( ( unsigned int * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_LONG:  /* long */
            hb_itemPutNL( pBaseVar->pItems + nIndex, *( ( long * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            hb_itemPutNL( pBaseVar->pItems + nIndex, ( long ) *( ( unsigned long * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_FLOAT:  /* float */
            hb_itemPutND( pBaseVar->pItems + nIndex, ( double ) *( ( float * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_DOUBLE:  /* double */
            hb_itemPutND( pBaseVar->pItems + nIndex, *( ( double * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_DOUBLE_PTR: /* double * */
         case CTYPE_VOID_PTR:   /* void * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         default:
         {
            HB_UINT  uiNestedSize /*, uiNestedAlign */;
            PHB_ITEM pID        = hb_itemPutNI( NULL, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
            PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

            hb_itemRelease( pID );

            if( ! HB_IS_OBJECT( pStructure ) )
            {
               hb_itemRelease( pStructure );
               hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               return pRet;
            }

            hb_objSendMsg( pStructure, "NALIGN", 0 );
            hb_objSendMsg( pStructure, "SizeOf", 0 );
            uiNestedSize = ( HB_UINT ) hb_parns( -1 );

            /* TraceLog( NULL, "* NestedSize: %i Offset: %i\n", uiNestedSize, uiOffset ); */

            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
            {
               /* printf( "Offset %i Pointer: %p\n", uiOffset, *( char ** ) ( (long ** )( Buffer + uiOffset ) ) ); */

               if( *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ) )
               {
                  PHB_BASEARRAY pBaseStructure  = pStructure->item.asArray.value;
                  PHB_ITEM      pInternalBuffer = pBaseStructure->pItems + pBaseStructure->nLen - 1;

                  if( ! bAdoptNested )
                  {
                     hb_itemPutCRawStatic( pInternalBuffer, *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ), uiNestedSize );
                  }
                  else
                  {
                     hb_itemPutCRaw( pInternalBuffer, *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ), uiNestedSize );
                  }

                  hb_objSendMsg( pStructure, "DEVALUE", 0 );
               }
               else
               {
                  /* hb_objSendMsg( pStructure, "RESET", 0 ); */
                  hb_itemClear( pStructure );
               }
            }
            else
            {
               PHB_BASEARRAY pBaseStructure  = pStructure->item.asArray.value;
               PHB_ITEM      pInternalBuffer = pBaseStructure->pItems + pBaseStructure->nLen - 1;
               HB_ITEM       Adopt;

               Adopt.type = HB_IT_LOGICAL;
               Adopt.item.asLogical.value = bAdoptNested;

               /* TraceLog( NULL, "Before Devalue\n" ); */

               hb_itemPutCRawStatic( pInternalBuffer, ( char * ) ( HB_BYTE * ) ( Buffer + uiOffset ), uiNestedSize );

               hb_objSendMsg( pStructure, "DEVALUE", 1, &Adopt );

               /* TraceLog( NULL, "After Devalue\n" ); */
            }

            hb_itemMove( pBaseVar->pItems + nIndex, pStructure );

            hb_itemRelease( pStructure );
         }
      }

      uiOffset += uiMemberSize;

      /* TraceLog( NULL, "AFTER Size: %i Offset: %i\n", uiMemberSize, uiOffset ); */
   }

   return pRet;
}

HB_FUNC( HB_STRUCTURETOARRAY )
{
   PHB_ITEM Structure = hb_param( 1, HB_IT_STRING );
   PHB_ITEM aDef      = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pAlign    = hb_param( 3, HB_IT_INTEGER );
   PHB_ITEM pAdopt    = hb_param( 4, HB_IT_LOGICAL );
   PHB_ITEM pRet      = hb_param( 5, HB_IT_ARRAY );
   HB_BOOL  bAdopt;

   if( Structure && aDef )
   {
      HB_BYTE * Buffer = ( HB_BYTE * ) Structure->item.asString.value;
      HB_UINT   uiAlign;

      if( pAlign )
         uiAlign = ( HB_BYTE ) pAlign->item.asInteger.value;
      else
         uiAlign = 8;

      if( pAdopt )
         bAdopt = pAdopt->item.asLogical.value;
      else
         bAdopt = HB_FALSE;

      hb_itemReturnForward( StructureToArray( Buffer, Structure->item.asString.length, aDef, uiAlign, bAdopt, pRet ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

HB_FUNC( HB_POINTER2STRING )
{
   PHB_ITEM pPointer = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pLen     = hb_param( 2, HB_IT_NUMERIC );

   if( HB_IS_POINTER( pPointer ) && pLen )
   {
      hb_retclen( ( char * ) hb_itemGetPtr( pPointer ), hb_itemGetNS( pLen ) );
   }
   else if( HB_IS_INTEGER( pPointer ) && pLen )
   {
      hb_retclen( ( char * ) ( HB_PTRDIFF ) hb_itemGetNI( pPointer ), hb_itemGetNS( pLen ) );
   }
   else if( HB_IS_LONG( pPointer ) && pLen )
   {
      hb_retclen( ( char * ) ( HB_PTRDIFF ) hb_itemGetNL( pPointer ), hb_itemGetNS( pLen ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

HB_FUNC( HB_STRING2POINTER )
{
   const char * pszString = hb_parc( 1 );

   if( pszString )
      hb_retptr( ( void * ) pszString );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}


HB_FUNC( __CSTR_COPYTO )
{
   static PHB_DYNS s_pVALUE = NULL;
   PHB_ITEM        pTarget  = hb_param( 1, HB_IT_ANY );
   PHB_ITEM        pStructure;
   void *          pPointer;

   if( s_pVALUE == NULL )
      s_pVALUE = hb_dynsymGetCase( "VALUE" );

   if( HB_IS_LONG( pTarget ) )
      pPointer = ( void * ) ( HB_PTRDIFF ) hb_itemGetNInt( pTarget );
#if UINT_MAX == ULONG_MAX
   else if( HB_IS_INTEGER( pTarget ) )
      pPointer = ( void * ) ( HB_PTRDIFF ) hb_itemGetNInt( pTarget );
#endif
   else if( HB_IS_POINTER( pTarget ) )
      pPointer = hb_itemGetPtr( pTarget );
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "C Structure:CopyTo()", 1, hb_paramError( 1 ) );
      return;
   }

   pStructure = hb_stackSelfItem();
   hb_vmPushDynSym( s_pVALUE );
   hb_vmPush( pStructure );
   hb_vmSend( 0 );

   memcpy( pPointer, hb_parc( -1 ), hb_arrayGetNI( pStructure, hb_arrayLen( pStructure ) - 2 ) );
}
