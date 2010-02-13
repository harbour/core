/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows DLL handling function
 *
 * Copyright 2009-2010 Viktor Szakats (harbour.01 syenar.hu)
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

#include "hbwin.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbstack.h"

#ifndef HB_WIN_NO_LEGACY
#define HB_WIN_NO_LEGACY
#endif
#undef HB_LEGACY_LEVEL3
#include "hbwin.ch"

#define _MASK_CALLCONV          0xF00000
#define _MASK_ENCODING          0x0F0000
#define _MASK_CTYPE             0x00FFFF

/* C raw return types */
#define _RETTYPERAW_INT32       1
#define _RETTYPERAW_INT64       2
#define _RETTYPERAW_DOUBLE      3
#define _RETTYPERAW_FLOAT       4

#define _DYNEXEC_MAXPARAM       15

static int hb_hbtoctype( int iHarbourType )
{
    switch( iHarbourType )
    {
       case HB_IT_NIL:
          return HB_WIN_DLL_CTYPE_VOID;

       case HB_IT_LOGICAL:
          return HB_WIN_DLL_CTYPE_BOOL;

       case HB_IT_INTEGER:
       case HB_IT_LONG:
       case HB_IT_DATE:
          return HB_WIN_DLL_CTYPE_LONG;

       case HB_IT_DOUBLE:
          return HB_WIN_DLL_CTYPE_DOUBLE;

       case HB_IT_STRING:
       case HB_IT_MEMO:
          return HB_WIN_DLL_CTYPE_CHAR_PTR;

       case HB_IT_POINTER:
          return HB_WIN_DLL_CTYPE_VOID_PTR;
    }

    return HB_WIN_DLL_CTYPE_DEFAULT;
}

#if defined( HB_ARCH_64BIT )

typedef struct
{
   void *    hString;
   int       iType;
   int       iEncoding;
   HB_BOOL   bByRef;
   HB_U64    nValue;
} HB_DYNARG, * PHB_DYNARG;

static HB_U64 hb_u64par( PHB_ITEM pParam, PHB_DYNARG pArg )
{
   HB_U64 r;

   switch( pArg->iType )
   {
      case HB_WIN_DLL_CTYPE_BOOL:
         pArg->nValue = hb_itemGetL( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->nValue : pArg->nValue;
         break;

      case HB_WIN_DLL_CTYPE_CHAR:
      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED:
         pArg->nValue = hb_itemGetNI( pParam ) & 0xFF;
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->nValue : pArg->nValue;
         break;

      case HB_WIN_DLL_CTYPE_SHORT:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED:
         pArg->nValue = hb_itemGetNI( pParam ) & 0xFFFF;
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->nValue : pArg->nValue;
         break;

      case HB_WIN_DLL_CTYPE_INT:
      case HB_WIN_DLL_CTYPE_INT_UNSIGNED:
      case HB_WIN_DLL_CTYPE_LONG:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED:
         pArg->nValue = hb_itemGetNL( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->nValue : pArg->nValue;
         break;

      case HB_WIN_DLL_CTYPE_LLONG:
      case HB_WIN_DLL_CTYPE_LLONG_UNSIGNED:
         pArg->nValue = hb_itemGetNInt( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->nValue : pArg->nValue;
         break;

      case HB_WIN_DLL_CTYPE_FLOAT:
      case HB_WIN_DLL_CTYPE_DOUBLE:
         HB_PUT_LE_DOUBLE( ( BYTE * ) &pArg->nValue, hb_itemGetND( pParam ) );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->nValue : pArg->nValue;
         break;

      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED_PTR:
         r = ( HB_PTRUINT ) hb_strunshare( &pArg->hString, hb_itemGetCPtr( pParam ), hb_itemGetCLen( pParam ) );
         pArg->nValue = r;
         break;

      case HB_WIN_DLL_CTYPE_CHAR_PTR:

         switch( pArg->iEncoding )
         {
            case HB_WIN_DLL_ENC_ASCII:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStr( pParam, hb_setGetOSCP(), &pArg->hString, &nLen );
               r = ( HB_PTRUINT ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_WIN_DLL_ENC_UTF8:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStrUTF8( pParam, &pArg->hString, &nLen );
               r = ( HB_PTRUINT ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_WIN_DLL_ENC_UTF16:
            {
               HB_SIZE nLen;
               const HB_WCHAR * s = hb_itemGetStrU16( pParam, HB_CDP_ENDIAN_NATIVE, &pArg->hString, &nLen );
               r = ( HB_PTRUINT ) hb_wstrunshare( &pArg->hString, s, nLen );
               break;
            }
            default:
               r = ( HB_PTRUINT ) hb_strunshare( &pArg->hString, hb_itemGetCPtr( pParam ), hb_itemGetCLen( pParam ) );
         }
         pArg->nValue = r;
         break;

      case HB_WIN_DLL_CTYPE_SHORT_PTR:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_INT_PTR:
      case HB_WIN_DLL_CTYPE_INT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_LONG_PTR:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_LLONG_PTR:
      case HB_WIN_DLL_CTYPE_LLONG_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_FLOAT_PTR:
      case HB_WIN_DLL_CTYPE_DOUBLE_PTR:
      case HB_WIN_DLL_CTYPE_BOOL_PTR:
      case HB_WIN_DLL_CTYPE_VOID_PTR:
      case HB_WIN_DLL_CTYPE_STRUCTURE_PTR:
         pArg->nValue = ( HB_PTRUINT ) hb_itemGetPtr( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->nValue : pArg->nValue;
         break;

      case HB_WIN_DLL_CTYPE_STRUCTURE:
         /* TODO */
         r = pArg->nValue = 0;
         break;

      case HB_WIN_DLL_CTYPE_VOID:
      default:
         r = pArg->nValue = 0;
   }

   return r;
}

static PHB_ITEM hb_u64ret( PHB_ITEM pItem, int iRetType, int iEncoding, HB_U64 nRetVal, HB_ISIZ nLen )
{
   switch( iRetType )
   {
      case HB_WIN_DLL_CTYPE_VOID:
         hb_itemClear( pItem );
         break;

      case HB_WIN_DLL_CTYPE_BOOL:
         hb_itemPutL( pItem, nRetVal != 0 );
         break;

      case HB_WIN_DLL_CTYPE_CHAR:
      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED:
      case HB_WIN_DLL_CTYPE_SHORT:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED:
      case HB_WIN_DLL_CTYPE_INT:
         hb_itemPutNI( pItem, ( int ) nRetVal );
         break;

      case HB_WIN_DLL_CTYPE_LONG:
         hb_itemPutNL( pItem, ( long ) nRetVal );
         break;

      case HB_WIN_DLL_CTYPE_INT_UNSIGNED:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED:
      case HB_WIN_DLL_CTYPE_LLONG:
      case HB_WIN_DLL_CTYPE_LLONG_UNSIGNED:
         hb_itemPutNInt( pItem, nRetVal );
         break;

      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED_PTR:
         hb_itemPutC( pItem, ( const char * ) nRetVal );
         break;

      case HB_WIN_DLL_CTYPE_CHAR_PTR:
         switch( iEncoding )
         {
            case HB_WIN_DLL_ENC_ASCII:
               if( nLen == -1 )
                  hb_itemPutStr( pItem, hb_setGetOSCP(), ( const char * ) nRetVal );
               else
                  hb_itemPutStrLen( pItem, hb_setGetOSCP(), ( const char * ) nRetVal, nLen );
               break;
            case HB_WIN_DLL_ENC_UTF8:
               if( nLen == -1 )
                  hb_itemPutStrUTF8( pItem, ( const char * ) nRetVal );
               else
                  hb_itemPutStrLenUTF8( pItem, ( const char * ) nRetVal, nLen );
               break;
            case HB_WIN_DLL_ENC_UTF16:
               if( nLen == -1 )
                  hb_itemPutStrU16( pItem, HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) nRetVal );
               else
                  hb_itemPutStrLenU16( pItem, HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) nRetVal, nLen );
               break;
            default:
               if( nLen == -1 )
                  hb_itemPutC( pItem, ( const char * ) nRetVal );
               else
                  hb_itemPutCL( pItem, ( const char * ) nRetVal, nLen );
         }
         break;

      case HB_WIN_DLL_CTYPE_INT_PTR:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_INT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_STRUCTURE_PTR:
      case HB_WIN_DLL_CTYPE_LONG_PTR:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_VOID_PTR:
      case HB_WIN_DLL_CTYPE_BOOL_PTR:
      case HB_WIN_DLL_CTYPE_FLOAT_PTR:
      case HB_WIN_DLL_CTYPE_DOUBLE_PTR:
         hb_itemPutPtr( pItem, ( void * ) nRetVal );
         break;

      case HB_WIN_DLL_CTYPE_FLOAT:
      case HB_WIN_DLL_CTYPE_DOUBLE:
         hb_itemPutND( pItem, HB_GET_LE_DOUBLE( ( HB_BYTE * ) &nRetVal ) );
         break;

      default:
         hb_itemPutNInt( pItem, nRetVal );
   }

   return pItem;
}

typedef HB_U64( * FX64_00 ) ( void );
typedef HB_U64( * FX64_01 ) ( HB_U64 );
typedef HB_U64( * FX64_02 ) ( HB_U64, HB_U64 );
typedef HB_U64( * FX64_03 ) ( HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_04 ) ( HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_05 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_06 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_07 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_08 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_09 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_10 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_11 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_12 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_13 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_14 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( * FX64_15 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );

#elif defined( HB_ARCH_32BIT )

typedef struct
{
   union
   {
      HB_U32 n32;
      HB_U64 n64;
      double nDB;
      float  nFL;
   } t;
} HB_DYNVAL;

typedef struct
{
   void *    hString;
   int       iType;
   int       iEncoding;
   HB_BOOL   bByRef;
   HB_DYNVAL value;
} HB_DYNARG, * PHB_DYNARG;

static void hb_u32par( PHB_ITEM pParam, PHB_DYNARG pArg, HB_U32 * r1, HB_U32 * r2, HB_BOOL * b64 )
{
   *b64 = HB_FALSE;

   switch( pArg->iType )
   {
      case HB_WIN_DLL_CTYPE_BOOL:
         pArg->value.t.n32 = hb_itemGetL( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_WIN_DLL_CTYPE_CHAR:
      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED:
         pArg->value.t.n32 = hb_itemGetNI( pParam ) & 0xFF;
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_WIN_DLL_CTYPE_SHORT:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED:
         pArg->value.t.n32 = hb_itemGetNI( pParam ) & 0xFFFF;
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_WIN_DLL_CTYPE_INT:
      case HB_WIN_DLL_CTYPE_INT_UNSIGNED:
      case HB_WIN_DLL_CTYPE_LONG:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED:
         pArg->value.t.n32 = hb_itemGetNL( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_WIN_DLL_CTYPE_LLONG:
      case HB_WIN_DLL_CTYPE_LLONG_UNSIGNED:
         pArg->value.t.n32 = hb_itemGetNL( pParam );
         if( pArg->bByRef )
            *r1 = ( HB_U32 ) &pArg->value.t.n64;
         else
         {
            *r1 = pArg->value.t.n64 & 0xFFFFFFFF;
            *r2 = ( pArg->value.t.n64 >> 32 );
            *b64 = HB_TRUE;
         }
         break;

      case HB_WIN_DLL_CTYPE_FLOAT:
         pArg->value.t.nFL = ( float ) hb_itemGetND( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.nFL : pArg->value.t.n32;
         break;

      case HB_WIN_DLL_CTYPE_DOUBLE:
         pArg->value.t.nDB = hb_itemGetND( pParam );
         if( pArg->bByRef )
            *r1 = ( HB_U32 ) &pArg->value.t.nDB;
         else
         {
            *r1 = pArg->value.t.n64 & 0xFFFFFFFF;
            *r2 = ( pArg->value.t.n64 >> 32 );
            *b64 = HB_TRUE;
         }
         break;

      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED_PTR:
         *r1 = ( HB_U32 ) hb_strunshare( &pArg->hString, hb_itemGetCPtr( pParam ), hb_itemGetCLen( pParam ) );
         pArg->value.t.n32 = *r1;
         break;

      case HB_WIN_DLL_CTYPE_CHAR_PTR:

         switch( pArg->iEncoding )
         {
            case HB_WIN_DLL_ENC_ASCII:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStr( pParam, hb_setGetOSCP(), &pArg->hString, &nLen );
               *r1 = ( HB_U32 ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_WIN_DLL_ENC_UTF8:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStrUTF8( pParam, &pArg->hString, &nLen );
               *r1 = ( HB_U32 ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_WIN_DLL_ENC_UTF16:
            {
               HB_SIZE nLen;
               const HB_WCHAR * s = hb_itemGetStrU16( pParam, HB_CDP_ENDIAN_NATIVE, &pArg->hString, &nLen );
               *r1 = ( HB_U32 ) hb_wstrunshare( &pArg->hString, s, nLen );
               break;
            }
            default:
               *r1 = ( HB_U32 ) hb_strunshare( &pArg->hString, hb_itemGetCPtr( pParam ), hb_itemGetCLen( pParam ) );
         }
         pArg->value.t.n32 = *r1;
         break;

      case HB_WIN_DLL_CTYPE_SHORT_PTR:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_INT_PTR:
      case HB_WIN_DLL_CTYPE_INT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_LONG_PTR:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_LLONG_PTR:
      case HB_WIN_DLL_CTYPE_LLONG_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_FLOAT_PTR:
      case HB_WIN_DLL_CTYPE_DOUBLE_PTR:
      case HB_WIN_DLL_CTYPE_BOOL_PTR:
      case HB_WIN_DLL_CTYPE_VOID_PTR:
      case HB_WIN_DLL_CTYPE_STRUCTURE_PTR:
         pArg->value.t.n32 = ( HB_U32 ) hb_itemGetPtr( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_WIN_DLL_CTYPE_STRUCTURE:
         /* TODO */
         *r1 = pArg->value.t.n32 = 0;
         break;

      case HB_WIN_DLL_CTYPE_VOID:
      default:
         *r1 = pArg->value.t.n32 = 0;
   }
}

static PHB_ITEM hb_u32ret( PHB_ITEM pItem, int iRetType, int iEncoding, HB_DYNVAL value, HB_ISIZ nLen )
{
   switch( iRetType )
   {
      case HB_WIN_DLL_CTYPE_VOID:
         hb_itemClear( pItem );
         break;

      case HB_WIN_DLL_CTYPE_BOOL:
         hb_itemPutL( pItem, value.t.n32 != 0 );
         break;

      case HB_WIN_DLL_CTYPE_CHAR:
      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED:
         hb_itemPutNI( pItem, ( int ) ( value.t.n32 & 0xFF ) );
         break;

      case HB_WIN_DLL_CTYPE_SHORT:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED:
      case HB_WIN_DLL_CTYPE_INT:
         hb_itemPutNI( pItem, ( int ) value.t.n32 );
         break;

      case HB_WIN_DLL_CTYPE_LONG:
         hb_itemPutNL( pItem, ( long ) value.t.n32 );
         break;

      case HB_WIN_DLL_CTYPE_INT_UNSIGNED:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED:
         hb_itemPutNInt( pItem, value.t.n32 );
         break;

      case HB_WIN_DLL_CTYPE_LLONG:
      case HB_WIN_DLL_CTYPE_LLONG_UNSIGNED:
         hb_itemPutNInt( pItem, value.t.n64 );
         break;

      case HB_WIN_DLL_CTYPE_CHAR_UNSIGNED_PTR:
         hb_itemPutC( pItem, ( const char * ) value.t.n32 );
         break;

      case HB_WIN_DLL_CTYPE_CHAR_PTR:

         switch( iEncoding )
         {
            case HB_WIN_DLL_ENC_ASCII:
               if( nLen == -1 )
                  hb_itemPutStr( pItem, hb_setGetOSCP(), ( const char * ) value.t.n32 );
               else
                  hb_itemPutStrLen( pItem, hb_setGetOSCP(), ( const char * ) value.t.n32, nLen );
               break;
            case HB_WIN_DLL_ENC_UTF8:
               if( nLen == -1 )
                  hb_itemPutStrUTF8( pItem, ( const char * ) value.t.n32 );
               else
                  hb_itemPutStrLenUTF8( pItem, ( const char * ) value.t.n32, nLen );
               break;
            case HB_WIN_DLL_ENC_UTF16:
               if( nLen == -1 )
                  hb_itemPutStrU16( pItem, HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) value.t.n32 );
               else
                  hb_itemPutStrLenU16( pItem, HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) value.t.n32, nLen );
               break;
            default:
               if( nLen == -1 )
                  hb_itemPutC( pItem, ( const char * ) value.t.n32 );
               else
                  hb_itemPutCL( pItem, ( const char * ) value.t.n32, nLen );
         }
         break;

      case HB_WIN_DLL_CTYPE_INT_PTR:
      case HB_WIN_DLL_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_INT_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_STRUCTURE_PTR:
      case HB_WIN_DLL_CTYPE_LONG_PTR:
      case HB_WIN_DLL_CTYPE_LONG_UNSIGNED_PTR:
      case HB_WIN_DLL_CTYPE_VOID_PTR:
      case HB_WIN_DLL_CTYPE_BOOL_PTR:
      case HB_WIN_DLL_CTYPE_FLOAT_PTR:
      case HB_WIN_DLL_CTYPE_DOUBLE_PTR:
         hb_itemPutPtr( pItem, ( void * ) value.t.n32 );
         break;

      case HB_WIN_DLL_CTYPE_FLOAT:
         hb_itemPutND( pItem, value.t.nFL );
         break;

      case HB_WIN_DLL_CTYPE_DOUBLE:
         hb_itemPutND( pItem, value.t.nDB );
         break;

      default:
         hb_itemPutNL( pItem, ( long ) value.t.n32 );
   }

   return pItem;
}

typedef HB_U32 ( _stdcall * FX86_S32P00 )( void );
typedef HB_U32 ( _stdcall * FX86_S32P01 )( HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P02 )( HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P03 )( HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _stdcall * FX86_S32P30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P00 )( void );
typedef HB_U64 ( _stdcall * FX86_S64P01 )( HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P02 )( HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P03 )( HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _stdcall * FX86_S64P30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP00 )( void );
typedef double ( _stdcall * FX86_SDBP01 )( HB_U32 );
typedef double ( _stdcall * FX86_SDBP02 )( HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP03 )( HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _stdcall * FX86_SDBP30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP00 )( void );
typedef float  ( _stdcall * FX86_SFLP01 )( HB_U32 );
typedef float  ( _stdcall * FX86_SFLP02 )( HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP03 )( HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _stdcall * FX86_SFLP30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P00 )( void );
typedef HB_U32 ( _cdecl   * FX86_C32P01 )( HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P02 )( HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P03 )( HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U32 ( _cdecl   * FX86_C32P30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P00 )( void );
typedef HB_U64 ( _cdecl   * FX86_C64P01 )( HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P02 )( HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P03 )( HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef HB_U64 ( _cdecl   * FX86_C64P30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP00 )( void );
typedef double ( _cdecl   * FX86_CDBP01 )( HB_U32 );
typedef double ( _cdecl   * FX86_CDBP02 )( HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP03 )( HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef double ( _cdecl   * FX86_CDBP30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP00 )( void );
typedef float  ( _cdecl   * FX86_CFLP01 )( HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP02 )( HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP03 )( HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP04 )( HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );
typedef float  ( _cdecl   * FX86_CFLP30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 );

#endif

void hbwin_dllCall( int iFuncFlags, FARPROC lpFunction, int iParams, int iFirst, int * piArgFlags )
{
   if( ! lpFunction )
      return;

#if defined( HB_ARCH_64BIT )
   {
      int iRetType  = iFuncFlags & _MASK_CTYPE;
      int iEncoding = iFuncFlags & _MASK_ENCODING;

      iParams -= iFirst - 1;

      if( iParams <= _DYNEXEC_MAXPARAM )
      {
         HB_U64 nRetVal = 0;
         HB_U64 rawpar[ _DYNEXEC_MAXPARAM ];
         HB_DYNARG * pArg;
         int tmp;

         if( iParams )
         {
            pArg = ( HB_DYNARG * ) hb_xgrab( iParams * sizeof( HB_DYNARG ) );
            memset( pArg, 0, iParams * sizeof( HB_DYNARG ) );
         }
         else
            pArg = NULL;

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            PHB_ITEM pParam = hb_param( iFirst + tmp, HB_IT_ANY );

            if( piArgFlags )
            {
               pArg[ tmp ].iType     = piArgFlags[ tmp ] & _MASK_CTYPE;
               pArg[ tmp ].iEncoding = piArgFlags[ tmp ] & _MASK_ENCODING;
            }
            else
            {
               pArg[ tmp ].iType     = HB_WIN_DLL_CTYPE_DEFAULT;
               pArg[ tmp ].iEncoding = iEncoding;
            }

            if( pArg[ tmp ].iType == HB_WIN_DLL_CTYPE_DEFAULT )
               pArg[ tmp ].iType = hb_hbtoctype( HB_ITEM_TYPE( pParam ) );

            pArg[ tmp ].bByRef = HB_ISBYREF( iFirst + tmp );

            rawpar[ tmp ] = hb_u64par( pParam, &pArg[ tmp ] );
         }

         switch( iParams )
         {
            case  0: nRetVal = ( ( FX64_00 ) *lpFunction )(); break;
            case  1: nRetVal = ( ( FX64_01 ) *lpFunction )( rawpar[ 0 ] ); break;
            case  2: nRetVal = ( ( FX64_02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
            case  3: nRetVal = ( ( FX64_03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
            case  4: nRetVal = ( ( FX64_04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
            case  5: nRetVal = ( ( FX64_05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
            case  6: nRetVal = ( ( FX64_06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
            case  7: nRetVal = ( ( FX64_07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
            case  8: nRetVal = ( ( FX64_08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
            case  9: nRetVal = ( ( FX64_09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
            case 10: nRetVal = ( ( FX64_10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
            case 11: nRetVal = ( ( FX64_11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
            case 12: nRetVal = ( ( FX64_12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
            case 13: nRetVal = ( ( FX64_13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
            case 14: nRetVal = ( ( FX64_14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
            case 15: nRetVal = ( ( FX64_15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
         }

         hb_u64ret( hb_stackReturnItem(), iRetType, iEncoding, nRetVal, -1 );

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            if( pArg[ tmp ].bByRef )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemParamStoreForward( ( HB_USHORT ) ( iFirst + tmp ),
                  hb_u64ret( pItem, pArg[ tmp ].iType, pArg[ tmp ].iEncoding, pArg[ tmp ].nValue, hb_parclen( iFirst + tmp ) ) );

               hb_itemRelease( pItem );
            }

            hb_strfree( pArg[ tmp ].hString );
         }

         if( pArg )
            hb_xfree( pArg );
      }
      else
         hb_errRT_BASE( EG_ARG, 2010, "A maximum of 15 parameters is supported", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
#elif defined( HB_ARCH_32BIT )
   {
      int iCallConv = iFuncFlags & _MASK_CALLCONV;
      int iRetType  = iFuncFlags & _MASK_CTYPE;
      int iEncoding = iFuncFlags & _MASK_ENCODING;

      iParams -= iFirst - 1;

      if( iParams <= _DYNEXEC_MAXPARAM )
      {
         int iRetTypeRaw;
         HB_DYNVAL ret;
         HB_DYNARG * pArg;
         int tmp;

         int iParamsRaw = 0;
         HB_U32 rawpar[ _DYNEXEC_MAXPARAM * 2 ];

         ret.t.n64 = 0;

         if( iRetType == HB_WIN_DLL_CTYPE_DOUBLE )
            iRetTypeRaw = _RETTYPERAW_DOUBLE;
         else if( iRetType == HB_WIN_DLL_CTYPE_FLOAT )
            iRetTypeRaw = _RETTYPERAW_FLOAT;
         else
            iRetTypeRaw = _RETTYPERAW_INT32;

         if( iParams )
         {
            pArg = ( HB_DYNARG * ) hb_xgrab( iParams * sizeof( HB_DYNARG ) );
            memset( pArg, 0, iParams * sizeof( HB_DYNARG ) );
         }
         else
            pArg = NULL;

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            PHB_ITEM pParam = hb_param( iFirst + tmp, HB_IT_ANY );

            HB_U32 r1;
            HB_U32 r2;
            HB_BOOL b64;

            if( piArgFlags )
            {
               pArg[ tmp ].iType     = piArgFlags[ tmp ] & _MASK_CTYPE;
               pArg[ tmp ].iEncoding = piArgFlags[ tmp ] & _MASK_ENCODING;
            }
            else
            {
               pArg[ tmp ].iType     = HB_WIN_DLL_CTYPE_DEFAULT;
               pArg[ tmp ].iEncoding = iEncoding;
            }

            if( pArg[ tmp ].iType == HB_WIN_DLL_CTYPE_DEFAULT )
               pArg[ tmp ].iType = hb_hbtoctype( HB_ITEM_TYPE( pParam ) );

            pArg[ tmp ].bByRef = HB_ISBYREF( iFirst + tmp );

            hb_u32par( pParam, &pArg[ tmp ], &r1, &r2, &b64 );

            /* TOFIX: Verify proper order. */
            rawpar[ iParamsRaw++ ] = r1;
            if( b64 )
               rawpar[ iParamsRaw++ ] = r2;
         }

         if( iCallConv == HB_WIN_DLL_CALLCONV_CDECL )
         {
            switch( iRetTypeRaw )
            {
            case _RETTYPERAW_INT32:
               switch( iParamsRaw )
               {
                  case  0: ret.t.n32 = ( ( FX86_C32P00 ) *lpFunction )(); break;
                  case  1: ret.t.n32 = ( ( FX86_C32P01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.n32 = ( ( FX86_C32P02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.n32 = ( ( FX86_C32P03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.n32 = ( ( FX86_C32P04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.n32 = ( ( FX86_C32P05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.n32 = ( ( FX86_C32P06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.n32 = ( ( FX86_C32P07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.n32 = ( ( FX86_C32P08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.n32 = ( ( FX86_C32P09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.n32 = ( ( FX86_C32P10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.n32 = ( ( FX86_C32P11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.n32 = ( ( FX86_C32P12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.n32 = ( ( FX86_C32P13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.n32 = ( ( FX86_C32P14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.n32 = ( ( FX86_C32P15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.n32 = ( ( FX86_C32P16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.n32 = ( ( FX86_C32P17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.n32 = ( ( FX86_C32P18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.n32 = ( ( FX86_C32P19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.n32 = ( ( FX86_C32P20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.n32 = ( ( FX86_C32P21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.n32 = ( ( FX86_C32P22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.n32 = ( ( FX86_C32P23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.n32 = ( ( FX86_C32P24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.n32 = ( ( FX86_C32P25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.n32 = ( ( FX86_C32P26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.n32 = ( ( FX86_C32P27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.n32 = ( ( FX86_C32P28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.n32 = ( ( FX86_C32P29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.n32 = ( ( FX86_C32P30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            case _RETTYPERAW_INT64:
               switch( iParamsRaw )
               {
                  case  0: ret.t.n64 = ( ( FX86_C64P00 ) *lpFunction )(); break;
                  case  1: ret.t.n64 = ( ( FX86_C64P01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.n64 = ( ( FX86_C64P02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.n64 = ( ( FX86_C64P03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.n64 = ( ( FX86_C64P04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.n64 = ( ( FX86_C64P05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.n64 = ( ( FX86_C64P06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.n64 = ( ( FX86_C64P07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.n64 = ( ( FX86_C64P08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.n64 = ( ( FX86_C64P09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.n64 = ( ( FX86_C64P10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.n64 = ( ( FX86_C64P11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.n64 = ( ( FX86_C64P12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.n64 = ( ( FX86_C64P13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.n64 = ( ( FX86_C64P14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.n64 = ( ( FX86_C64P15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.n64 = ( ( FX86_C64P16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.n64 = ( ( FX86_C64P17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.n64 = ( ( FX86_C64P18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.n64 = ( ( FX86_C64P19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.n64 = ( ( FX86_C64P20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.n64 = ( ( FX86_C64P21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.n64 = ( ( FX86_C64P22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.n64 = ( ( FX86_C64P23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.n64 = ( ( FX86_C64P24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.n64 = ( ( FX86_C64P25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.n64 = ( ( FX86_C64P26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.n64 = ( ( FX86_C64P27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.n64 = ( ( FX86_C64P28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.n64 = ( ( FX86_C64P29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.n64 = ( ( FX86_C64P30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            case _RETTYPERAW_DOUBLE:
               switch( iParamsRaw )
               {
                  case  0: ret.t.nDB = ( ( FX86_CDBP00 ) *lpFunction )(); break;
                  case  1: ret.t.nDB = ( ( FX86_CDBP01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.nDB = ( ( FX86_CDBP02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.nDB = ( ( FX86_CDBP03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.nDB = ( ( FX86_CDBP04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.nDB = ( ( FX86_CDBP05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.nDB = ( ( FX86_CDBP06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.nDB = ( ( FX86_CDBP07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.nDB = ( ( FX86_CDBP08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.nDB = ( ( FX86_CDBP09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.nDB = ( ( FX86_CDBP10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.nDB = ( ( FX86_CDBP11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.nDB = ( ( FX86_CDBP12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.nDB = ( ( FX86_CDBP13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.nDB = ( ( FX86_CDBP14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.nDB = ( ( FX86_CDBP15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.nDB = ( ( FX86_CDBP16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.nDB = ( ( FX86_CDBP17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.nDB = ( ( FX86_CDBP18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.nDB = ( ( FX86_CDBP19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.nDB = ( ( FX86_CDBP20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.nDB = ( ( FX86_CDBP21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.nDB = ( ( FX86_CDBP22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.nDB = ( ( FX86_CDBP23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.nDB = ( ( FX86_CDBP24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.nDB = ( ( FX86_CDBP25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.nDB = ( ( FX86_CDBP26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.nDB = ( ( FX86_CDBP27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.nDB = ( ( FX86_CDBP28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.nDB = ( ( FX86_CDBP29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.nDB = ( ( FX86_CDBP30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            case _RETTYPERAW_FLOAT:
               switch( iParamsRaw )
               {
                  case  0: ret.t.nFL = ( ( FX86_CFLP00 ) *lpFunction )(); break;
                  case  1: ret.t.nFL = ( ( FX86_CFLP01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.nFL = ( ( FX86_CFLP02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.nFL = ( ( FX86_CFLP03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.nFL = ( ( FX86_CFLP04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.nFL = ( ( FX86_CFLP05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.nFL = ( ( FX86_CFLP06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.nFL = ( ( FX86_CFLP07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.nFL = ( ( FX86_CFLP08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.nFL = ( ( FX86_CFLP09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.nFL = ( ( FX86_CFLP10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.nFL = ( ( FX86_CFLP11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.nFL = ( ( FX86_CFLP12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.nFL = ( ( FX86_CFLP13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.nFL = ( ( FX86_CFLP14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.nFL = ( ( FX86_CFLP15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.nFL = ( ( FX86_CFLP16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.nFL = ( ( FX86_CFLP17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.nFL = ( ( FX86_CFLP18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.nFL = ( ( FX86_CFLP19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.nFL = ( ( FX86_CFLP20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.nFL = ( ( FX86_CFLP21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.nFL = ( ( FX86_CFLP22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.nFL = ( ( FX86_CFLP23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.nFL = ( ( FX86_CFLP24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.nFL = ( ( FX86_CFLP25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.nFL = ( ( FX86_CFLP26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.nFL = ( ( FX86_CFLP27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.nFL = ( ( FX86_CFLP28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.nFL = ( ( FX86_CFLP29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.nFL = ( ( FX86_CFLP30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            }
         }
         else /* stdcall */
         {
            switch( iRetTypeRaw )
            {
            case _RETTYPERAW_INT32:
               switch( iParamsRaw )
               {
                  case  0: ret.t.n32 = ( ( FX86_S32P00 ) *lpFunction )(); break;
                  case  1: ret.t.n32 = ( ( FX86_S32P01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.n32 = ( ( FX86_S32P02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.n32 = ( ( FX86_S32P03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.n32 = ( ( FX86_S32P04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.n32 = ( ( FX86_S32P05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.n32 = ( ( FX86_S32P06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.n32 = ( ( FX86_S32P07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.n32 = ( ( FX86_S32P08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.n32 = ( ( FX86_S32P09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.n32 = ( ( FX86_S32P10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.n32 = ( ( FX86_S32P11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.n32 = ( ( FX86_S32P12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.n32 = ( ( FX86_S32P13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.n32 = ( ( FX86_S32P14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.n32 = ( ( FX86_S32P15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.n32 = ( ( FX86_S32P16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.n32 = ( ( FX86_S32P17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.n32 = ( ( FX86_S32P18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.n32 = ( ( FX86_S32P19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.n32 = ( ( FX86_S32P20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.n32 = ( ( FX86_S32P21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.n32 = ( ( FX86_S32P22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.n32 = ( ( FX86_S32P23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.n32 = ( ( FX86_S32P24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.n32 = ( ( FX86_S32P25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.n32 = ( ( FX86_S32P26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.n32 = ( ( FX86_S32P27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.n32 = ( ( FX86_S32P28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.n32 = ( ( FX86_S32P29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.n32 = ( ( FX86_S32P30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            case _RETTYPERAW_INT64:
               switch( iParamsRaw )
               {
                  case  0: ret.t.n64 = ( ( FX86_S64P00 ) *lpFunction )(); break;
                  case  1: ret.t.n64 = ( ( FX86_S64P01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.n64 = ( ( FX86_S64P02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.n64 = ( ( FX86_S64P03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.n64 = ( ( FX86_S64P04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.n64 = ( ( FX86_S64P05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.n64 = ( ( FX86_S64P06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.n64 = ( ( FX86_S64P07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.n64 = ( ( FX86_S64P08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.n64 = ( ( FX86_S64P09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.n64 = ( ( FX86_S64P10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.n64 = ( ( FX86_S64P11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.n64 = ( ( FX86_S64P12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.n64 = ( ( FX86_S64P13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.n64 = ( ( FX86_S64P14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.n64 = ( ( FX86_S64P15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.n64 = ( ( FX86_S64P16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.n64 = ( ( FX86_S64P17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.n64 = ( ( FX86_S64P18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.n64 = ( ( FX86_S64P19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.n64 = ( ( FX86_S64P20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.n64 = ( ( FX86_S64P21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.n64 = ( ( FX86_S64P22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.n64 = ( ( FX86_S64P23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.n64 = ( ( FX86_S64P24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.n64 = ( ( FX86_S64P25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.n64 = ( ( FX86_S64P26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.n64 = ( ( FX86_S64P27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.n64 = ( ( FX86_S64P28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.n64 = ( ( FX86_S64P29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.n64 = ( ( FX86_S64P30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            case _RETTYPERAW_DOUBLE:
               switch( iParamsRaw )
               {
                  case  0: ret.t.nDB = ( ( FX86_SDBP00 ) *lpFunction )(); break;
                  case  1: ret.t.nDB = ( ( FX86_SDBP01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.nDB = ( ( FX86_SDBP02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.nDB = ( ( FX86_SDBP03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.nDB = ( ( FX86_SDBP04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.nDB = ( ( FX86_SDBP05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.nDB = ( ( FX86_SDBP06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.nDB = ( ( FX86_SDBP07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.nDB = ( ( FX86_SDBP08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.nDB = ( ( FX86_SDBP09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.nDB = ( ( FX86_SDBP10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.nDB = ( ( FX86_SDBP11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.nDB = ( ( FX86_SDBP12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.nDB = ( ( FX86_SDBP13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.nDB = ( ( FX86_SDBP14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.nDB = ( ( FX86_SDBP15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.nDB = ( ( FX86_SDBP16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.nDB = ( ( FX86_SDBP17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.nDB = ( ( FX86_SDBP18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.nDB = ( ( FX86_SDBP19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.nDB = ( ( FX86_SDBP20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.nDB = ( ( FX86_SDBP21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.nDB = ( ( FX86_SDBP22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.nDB = ( ( FX86_SDBP23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.nDB = ( ( FX86_SDBP24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.nDB = ( ( FX86_SDBP25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.nDB = ( ( FX86_SDBP26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.nDB = ( ( FX86_SDBP27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.nDB = ( ( FX86_SDBP28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.nDB = ( ( FX86_SDBP29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.nDB = ( ( FX86_SDBP30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            case _RETTYPERAW_FLOAT:
               switch( iParamsRaw )
               {
                  case  0: ret.t.nFL = ( ( FX86_SFLP00 ) *lpFunction )(); break;
                  case  1: ret.t.nFL = ( ( FX86_SFLP01 ) *lpFunction )( rawpar[ 0 ] ); break;
                  case  2: ret.t.nFL = ( ( FX86_SFLP02 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break;
                  case  3: ret.t.nFL = ( ( FX86_SFLP03 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break;
                  case  4: ret.t.nFL = ( ( FX86_SFLP04 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break;
                  case  5: ret.t.nFL = ( ( FX86_SFLP05 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break;
                  case  6: ret.t.nFL = ( ( FX86_SFLP06 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break;
                  case  7: ret.t.nFL = ( ( FX86_SFLP07 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break;
                  case  8: ret.t.nFL = ( ( FX86_SFLP08 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break;
                  case  9: ret.t.nFL = ( ( FX86_SFLP09 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break;
                  case 10: ret.t.nFL = ( ( FX86_SFLP10 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break;
                  case 11: ret.t.nFL = ( ( FX86_SFLP11 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break;
                  case 12: ret.t.nFL = ( ( FX86_SFLP12 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break;
                  case 13: ret.t.nFL = ( ( FX86_SFLP13 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break;
                  case 14: ret.t.nFL = ( ( FX86_SFLP14 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break;
                  case 15: ret.t.nFL = ( ( FX86_SFLP15 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break;
                  case 16: ret.t.nFL = ( ( FX86_SFLP16 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break;
                  case 17: ret.t.nFL = ( ( FX86_SFLP17 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break;
                  case 18: ret.t.nFL = ( ( FX86_SFLP18 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break;
                  case 19: ret.t.nFL = ( ( FX86_SFLP19 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break;
                  case 20: ret.t.nFL = ( ( FX86_SFLP20 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break;
                  case 21: ret.t.nFL = ( ( FX86_SFLP21 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break;
                  case 22: ret.t.nFL = ( ( FX86_SFLP22 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break;
                  case 23: ret.t.nFL = ( ( FX86_SFLP23 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break;
                  case 24: ret.t.nFL = ( ( FX86_SFLP24 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break;
                  case 25: ret.t.nFL = ( ( FX86_SFLP25 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break;
                  case 26: ret.t.nFL = ( ( FX86_SFLP26 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break;
                  case 27: ret.t.nFL = ( ( FX86_SFLP27 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break;
                  case 28: ret.t.nFL = ( ( FX86_SFLP28 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break;
                  case 29: ret.t.nFL = ( ( FX86_SFLP29 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break;
                  case 30: ret.t.nFL = ( ( FX86_SFLP30 ) *lpFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break;
               }
               break;
            }
         }

         hb_u32ret( hb_stackReturnItem(), iRetType, iEncoding, ret, -1 );

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            if( pArg[ tmp ].bByRef )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemParamStoreForward( ( HB_USHORT ) ( iFirst + tmp ),
                  hb_u32ret( pItem, pArg[ tmp ].iType, pArg[ tmp ].iEncoding, pArg[ tmp ].value, hb_parclen( iFirst + tmp ) ) );

               hb_itemRelease( pItem );
            }

            hb_strfree( pArg[ tmp ].hString );
         }

         if( pArg )
            hb_xfree( pArg );
      }
      else
         hb_errRT_BASE( EG_ARG, 2010, "A maximum of 15 parameters is supported", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
#else
   HB_SYMBOL_UNUSED( iFuncFlags );
   HB_SYMBOL_UNUSED( iParams );
   HB_SYMBOL_UNUSED( iFirst );
   HB_SYMBOL_UNUSED( piArgFlags );
#endif
}

/* ------------------------------------------------------------------ */

FARPROC hbwin_getprocaddress( HMODULE hDLL, PHB_ITEM pParam, HB_BOOL * pbUNICODE )
{
#if defined( HB_OS_WIN_CE )
   void * hStr;
   HB_SIZE nLen;
   LPCWSTR szProc = hb_itemGetStrU16( pParam, HB_CDP_ENDIAN_NATIVE, &hStr, &nLen );
   FARPROC lpFunction = GetProcAddress( hDLL, szProc ? szProc :
                  ( LPCWSTR ) ( HB_PTRDIFF ) ( hb_itemGetNI( pParam ) & 0x0FFFF ) );

   if( ! lpFunction && szProc ) /* try with WIDE suffix? */
   {
      LPWSTR pszProcW = ( LPWSTR ) hb_xgrab( ( nLen + 2 ) * sizeof( WCHAR ) );
      memcpy( pszProcW, szProc, nLen * sizeof( WCHAR ) );
      pszProcW[ nLen++ ] = L'W';
      pszProcW[ nLen++ ] = 0;
      lpFunction = GetProcAddress( hDLL, pszProcW );
      hb_xfree( pszProcW );
   }
   hb_strfree( hStr );

   if( pbUNICODE )
      *pbUNICODE = HB_TRUE;
#else
   const char * szProc = hb_itemGetCPtr( pParam );
   FARPROC lpFunction = GetProcAddress( hDLL, szProc ? szProc :
                  ( LPCSTR ) ( HB_PTRDIFF ) ( hb_itemGetNI( pParam ) & 0x0FFFF ) );

   if( pbUNICODE )
      *pbUNICODE = HB_FALSE;

#if defined( UNICODE )
   if( ! lpFunction && szProc ) /* try with WIDE suffix? */
   {
      char * pszFuncName = hb_xstrcpy( NULL, szProc, "W", NULL );
      lpFunction = GetProcAddress( hDLL, pszFuncName );
      hb_xfree( pszFuncName );
      if( pbUNICODE )
         *pbUNICODE = HB_TRUE;
   }
#endif

   if( ! lpFunction && szProc ) /* try with ANSI suffix? */
   {
      char * pszFuncName = hb_xstrcpy( NULL, szProc, "A", NULL );
      lpFunction = GetProcAddress( hDLL, pszFuncName );
      hb_xfree( pszFuncName );
      if( pbUNICODE )
         *pbUNICODE = HB_FALSE;
   }
#endif
   return lpFunction;
}

HB_FUNC( WIN_DLLCALL )
{
   PHB_ITEM pParam = hb_param( 1, HB_IT_POINTER | HB_IT_ARRAY );
   int * piArgFlags = NULL;
   int iFuncFlags = 0;
   HB_BOOL bFreeDLL = HB_FALSE;

   HMODULE hDLL = NULL;
   FARPROC lpFunction = NULL;

   if( pParam )
   {
      if( HB_IS_ARRAY( pParam ) )
      {
         HB_SIZE nLen = hb_arrayLen( pParam );

         if( nLen >= 1 )
         {
            PHB_ITEM pFunction = hb_arrayGetItemPtr( pParam, 1 );
            HB_SIZE nBasePos = 2;

            if( HB_IS_POINTER( pFunction ) )
               lpFunction = ( FARPROC ) hb_itemGetPtr( pFunction );
            else if( ( HB_IS_NUMERIC( pFunction ) || HB_IS_STRING( pFunction ) ) && nLen >= nBasePos )
            {
               PHB_ITEM pLibrary = hb_arrayGetItemPtr( pParam, nBasePos );

               if( HB_IS_STRING( pLibrary ) )
               {
                  void * hFileName;
                  hDLL = LoadLibrary( HB_ITEMGETSTR( pLibrary, &hFileName, NULL ) );
                  hb_strfree( hFileName );
                  if( ( HB_PTRDIFF ) hDLL < 32 )
                     hDLL = NULL;
                  else
                     bFreeDLL = HB_TRUE;
               }
               else if( HB_IS_POINTER( pLibrary ) )
                  hDLL = ( HMODULE ) hb_itemGetPtr( pLibrary );

               if( hDLL )
               {
                  HB_BOOL bUNICODE;
                  lpFunction = hbwin_getprocaddress( hDLL, pFunction, &bUNICODE );
                  if( bUNICODE )
                     iFuncFlags |= HB_WIN_DLL_ENC_UTF16;
               }

               ++nBasePos;
            }

            /* Function flags */
            if( nLen >= nBasePos )
               iFuncFlags = hb_arrayGetNI( pParam, nBasePos );

            ++nBasePos;

            /* Argument flags */
            if( nLen >= nBasePos )
            {
               HB_SIZE nPos;
               HB_SIZE nArgCount = hb_pcount() - 1;

               piArgFlags = ( int * ) hb_xgrab( sizeof( int ) * nArgCount );

               for( nPos = 0; nPos < nArgCount; ++nPos )
                  piArgFlags[ nPos ] = ( ( nPos + nBasePos ) <= nLen && HB_IS_NUMERIC( hb_arrayGetItemPtr( pParam, nPos + nBasePos ) ) ) ? hb_arrayGetNI( pParam, nPos + nBasePos ) : HB_WIN_DLL_CTYPE_DEFAULT;
            }
         }
      }
      else if( HB_IS_POINTER( pParam ) )
         lpFunction = ( FARPROC ) hb_itemGetPtr( pParam );
   }

   hbwin_dllCall( iFuncFlags, lpFunction, hb_pcount(), 2, piArgFlags );

   if( piArgFlags )
      hb_xfree( piArgFlags );

   if( bFreeDLL )
      FreeLibrary( hDLL );
}
