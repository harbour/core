/*
 * Harbour Project source code:
 * Dynamic call support
 *
 * Copyright 2009-2010 Viktor Szakats (vszakats.net/harbour)
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
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbset.h"
#include "hbstack.h"

#include "hbdyn.ch"

#define _MASK_CTYPE         0x000FFFF
#define _MASK_ENCODING      0x00F0000
#define _MASK_CALLCONV      0x0F00000
#define _MASK_OPTIONS       0xF000000

/* C raw return types */
#define _RETTYPERAW_INT32   1
#define _RETTYPERAW_INT64   2
#define _RETTYPERAW_DOUBLE  3
#define _RETTYPERAW_FLOAT   4

#define _DYNEXEC_MAXPARAM   15

typedef void ( *PHB_DYNADDR )( void );

static int hb_hbtoctype( int iHarbourType )
{
   switch( iHarbourType )
   {
      case HB_IT_NIL:
         return HB_DYN_CTYPE_VOID;

      case HB_IT_LOGICAL:
         return HB_DYN_CTYPE_BOOL;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DATE:
         return HB_DYN_CTYPE_LONG;

      case HB_IT_DOUBLE:
         return HB_DYN_CTYPE_DOUBLE;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         return HB_DYN_CTYPE_CHAR_PTR;

      case HB_IT_POINTER:
         return HB_DYN_CTYPE_VOID_PTR;
   }

   return HB_DYN_CTYPE_DEFAULT;
}

#if defined( HB_ARCH_64BIT )

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
   int       iOptions;
   HB_BOOL   bRawBuffer;
   HB_BOOL   bByRef;
   HB_DYNVAL value;
} HB_DYNARG, * PHB_DYNARG;

static HB_U64 hb_u64par( PHB_ITEM pParam, PHB_DYNARG pArg )
{
   HB_U64 r;

   switch( pArg->iType )
   {
      case HB_DYN_CTYPE_BOOL:
         pArg->value.t.n64 = hb_itemGetL( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_CHAR:
         pArg->value.t.n64 = ( char ) hb_itemGetNI( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED:
         pArg->value.t.n64 = ( unsigned char ) hb_itemGetNI( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_SHORT:
         pArg->value.t.n64 = ( short ) hb_itemGetNI( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_SHORT_UNSIGNED:
         pArg->value.t.n64 = ( unsigned short ) hb_itemGetNI( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_INT:
         pArg->value.t.n64 = hb_itemGetNI( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_INT_UNSIGNED:
         pArg->value.t.n64 = ( unsigned int ) hb_itemGetNInt( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_LONG:
         pArg->value.t.n64 = hb_itemGetNL( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_LONG_UNSIGNED:
         pArg->value.t.n64 = ( unsigned long ) hb_itemGetNInt( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_LLONG:
         pArg->value.t.n64 = hb_itemGetNInt( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_LLONG_UNSIGNED:
         /* TOFIX: Digits are lost. */
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
         pArg->value.t.n64 = ( HB_MAXUINT ) hb_itemGetNInt( pParam );
#else
         pArg->value.t.n64 = ( HB_ULONGLONG ) hb_itemGetNInt( pParam );
#endif
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_FLOAT:
         /* TOFIX */

      case HB_DYN_CTYPE_DOUBLE:
         HB_PUT_LE_DOUBLE( ( HB_BYTE * ) &pArg->value.t.n64, hb_itemGetND( pParam ) );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED_PTR:
      case HB_DYN_CTYPE_STRUCTURE:
      {
         HB_SIZE nLen = hb_itemGetCLen( pParam );
         pArg->hString = hb_xgrab( nLen + sizeof( char ) );
         pArg->bRawBuffer = HB_TRUE;
         memcpy( ( char * ) pArg->hString, hb_itemGetCPtr( pParam ), nLen );
         ( ( char * ) pArg->hString )[ nLen ] = '\0';
         r = ( HB_PTRUINT ) pArg->hString;
         pArg->value.t.n64 = r;
         break;
      }
      case HB_DYN_CTYPE_CHAR_PTR:

         switch( pArg->iEncoding )
         {
            case HB_DYN_ENC_ASCII:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStr( pParam, hb_setGetOSCP(), &pArg->hString, &nLen );
               r = ( HB_PTRUINT ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_DYN_ENC_UTF8:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStrUTF8( pParam, &pArg->hString, &nLen );
               r = ( HB_PTRUINT ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_DYN_ENC_UTF16:
            {
               HB_SIZE nLen;
               const HB_WCHAR * s = hb_itemGetStrU16( pParam, HB_CDP_ENDIAN_NATIVE, &pArg->hString, &nLen );
               r = ( HB_PTRUINT ) hb_wstrunshare( &pArg->hString, s, nLen );
               break;
            }
            default:
            {
               HB_SIZE nLen = hb_itemGetCLen( pParam );
               pArg->hString = hb_xgrab( nLen + sizeof( char ) );
               pArg->bRawBuffer = HB_TRUE;
               memcpy( ( char * ) pArg->hString, hb_itemGetCPtr( pParam ), nLen );
               ( ( char * ) pArg->hString )[ nLen ] = '\0';
               r = ( HB_PTRUINT ) pArg->hString;
               break;
            }
         }
         pArg->value.t.n64 = r;
         break;

      case HB_DYN_CTYPE_VOID_PTR:
      case HB_DYN_CTYPE_BOOL_PTR:
      case HB_DYN_CTYPE_SHORT_PTR:
      case HB_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_INT_PTR:
      case HB_DYN_CTYPE_INT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LONG_PTR:
      case HB_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LLONG_PTR:
      case HB_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_FLOAT_PTR:
      case HB_DYN_CTYPE_DOUBLE_PTR:
      case HB_DYN_CTYPE_STRUCTURE_PTR:
         pArg->value.t.n64 = ( HB_PTRUINT ) hb_itemGetPtr( pParam );
         r = pArg->bByRef ? ( HB_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case HB_DYN_CTYPE_VOID:
      default:
         r = pArg->value.t.n64 = 0;
   }

   return r;
}

static PHB_ITEM hb_u64ret( PHB_ITEM pItem, int iRetType, int iEncoding, HB_DYNVAL value, HB_ISIZ nLen )
{
   switch( iRetType )
   {
      case HB_DYN_CTYPE_VOID:
         hb_itemClear( pItem );
         break;

      case HB_DYN_CTYPE_BOOL:
         hb_itemPutL( pItem, value.t.n64 != 0 );
         break;

      case HB_DYN_CTYPE_CHAR:
         hb_itemPutNI( pItem, ( char ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED:
         hb_itemPutNI( pItem, ( unsigned char ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_SHORT:
         hb_itemPutNI( pItem, ( short ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_SHORT_UNSIGNED:
         hb_itemPutNI( pItem, ( unsigned short ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_INT:
         hb_itemPutNI( pItem, ( int ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_INT_UNSIGNED:
         hb_itemPutNInt( pItem, ( unsigned int ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_LONG:
         hb_itemPutNL( pItem, ( long ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_LONG_UNSIGNED:
         hb_itemPutNInt( pItem, ( unsigned long ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_LLONG:
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
         hb_itemPutNInt( pItem, ( HB_MAXINT ) value.t.n64 );
#else
         hb_itemPutNInt( pItem, ( HB_LONGLONG ) value.t.n64 );
#endif
         break;

      case HB_DYN_CTYPE_LLONG_UNSIGNED:
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
         hb_itemPutNInt( pItem, ( HB_MAXUINT ) value.t.n64 );
#else
         hb_itemPutNInt( pItem, ( HB_ULONGLONG ) value.t.n64 );
#endif
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED_PTR:
         if( nLen == -1 )
            hb_itemPutC( pItem, ( const char * ) value.t.n64 );
         else
            hb_itemPutCL( pItem, ( const char * ) value.t.n64, nLen );
         break;

      case HB_DYN_CTYPE_CHAR_PTR:
         switch( iEncoding )
         {
            case HB_DYN_ENC_ASCII:
               if( nLen == -1 )
                  hb_itemPutStr( pItem, hb_setGetOSCP(), ( const char * ) value.t.n64 );
               else
                  hb_itemPutStrLen( pItem, hb_setGetOSCP(), ( const char * ) value.t.n64, nLen );
               break;
            case HB_DYN_ENC_UTF8:
               if( nLen == -1 )
                  hb_itemPutStrUTF8( pItem, ( const char * ) value.t.n64 );
               else
                  hb_itemPutStrLenUTF8( pItem, ( const char * ) value.t.n64, nLen );
               break;
            case HB_DYN_ENC_UTF16:
               if( nLen == -1 )
                  hb_itemPutStrU16( pItem, HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) value.t.n64 );
               else
                  hb_itemPutStrLenU16( pItem, HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) value.t.n64, nLen );
               break;
            default:
               if( nLen == -1 )
                  hb_itemPutC( pItem, ( const char * ) value.t.n64 );
               else
                  hb_itemPutCL( pItem, ( const char * ) value.t.n64, nLen );
         }
         break;

      case HB_DYN_CTYPE_VOID_PTR:
      case HB_DYN_CTYPE_BOOL_PTR:
      case HB_DYN_CTYPE_SHORT_PTR:
      case HB_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_INT_PTR:
      case HB_DYN_CTYPE_INT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LONG_PTR:
      case HB_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LLONG_PTR:
      case HB_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_FLOAT_PTR:
      case HB_DYN_CTYPE_DOUBLE_PTR:
      case HB_DYN_CTYPE_STRUCTURE_PTR:
         hb_itemPutPtr( pItem, ( void * ) value.t.n64 );
         break;

      case HB_DYN_CTYPE_FLOAT:
         hb_itemPutND( pItem, value.t.nFL );
         break;

      case HB_DYN_CTYPE_DOUBLE:
         hb_itemPutND( pItem, value.t.nDB );
         break;

      default:
         hb_itemPutNInt( pItem, value.t.n64 );
   }

   return pItem;
}

#define HB_DYN_CTYPE_DECL( _ret_, _type_ ) \
   typedef _ret_ ( *_type_##P00 )( void ); \
   typedef _ret_ ( *_type_##P01 )( HB_U64 ); \
   typedef _ret_ ( *_type_##P02 )( HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P03 )( HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P04 )( HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P05 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P06 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P07 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P08 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P09 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P10 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P11 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P12 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P13 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P14 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 ); \
   typedef _ret_ ( *_type_##P15 )( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 )

#define HB_DYN_FUN_CALL( pcount, _ret_, _type_ ) \
   do \
   { \
      switch( pcount ) \
      { \
         case  0: ret.t._ret_ = ( ( _type_##P00 ) * pFunction )( ); break; \
         case  1: ret.t._ret_ = ( ( _type_##P01 ) * pFunction )( rawpar[ 0 ] ); break; \
         case  2: ret.t._ret_ = ( ( _type_##P02 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break; \
         case  3: ret.t._ret_ = ( ( _type_##P03 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break; \
         case  4: ret.t._ret_ = ( ( _type_##P04 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break; \
         case  5: ret.t._ret_ = ( ( _type_##P05 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break; \
         case  6: ret.t._ret_ = ( ( _type_##P06 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break; \
         case  7: ret.t._ret_ = ( ( _type_##P07 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break; \
         case  8: ret.t._ret_ = ( ( _type_##P08 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break; \
         case  9: ret.t._ret_ = ( ( _type_##P09 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break; \
         case 10: ret.t._ret_ = ( ( _type_##P10 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break; \
         case 11: ret.t._ret_ = ( ( _type_##P11 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break; \
         case 12: ret.t._ret_ = ( ( _type_##P12 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break; \
         case 13: ret.t._ret_ = ( ( _type_##P13 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break; \
         case 14: ret.t._ret_ = ( ( _type_##P14 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break; \
         case 15: ret.t._ret_ = ( ( _type_##P15 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break; \
      } \
   } \
   while( 0 )

HB_DYN_CTYPE_DECL( HB_U64, FX64_64 );
HB_DYN_CTYPE_DECL( double, FX64_DB );
HB_DYN_CTYPE_DECL( float,  FX64_FL );

#elif defined( HB_ARCH_32BIT )

typedef struct
{
   union
   {
      HB_U32 n32;
#if ! defined( HB_LONG_LONG_OFF )
      HB_U64 n64;
#endif
      double nDB;
      float  nFL;
   } t;
} HB_DYNVAL;

typedef struct
{
   void *    hString;
   int       iType;
   int       iEncoding;
   int       iOptions;
   HB_BOOL   bRawBuffer;
   HB_BOOL   bByRef;
   HB_DYNVAL value;
} HB_DYNARG, * PHB_DYNARG;

static void hb_u32par( PHB_ITEM pParam, PHB_DYNARG pArg, HB_U32 * r1, HB_U32 * r2, HB_BOOL * b64 )
{
   *b64 = HB_FALSE;
   *r2 = 0;

   switch( pArg->iType )
   {
      case HB_DYN_CTYPE_BOOL:
         pArg->value.t.n32 = hb_itemGetL( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_CHAR:
         pArg->value.t.n32 = ( char ) hb_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED:
         pArg->value.t.n32 = ( unsigned char ) hb_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_SHORT:
         pArg->value.t.n32 = ( short ) hb_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_SHORT_UNSIGNED:
         pArg->value.t.n32 = ( unsigned short ) hb_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_INT:
         pArg->value.t.n32 = hb_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_INT_UNSIGNED:
         pArg->value.t.n32 = ( unsigned int ) hb_itemGetNInt( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_LONG:
         pArg->value.t.n32 = hb_itemGetNL( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_LONG_UNSIGNED:
         pArg->value.t.n32 = ( unsigned long ) hb_itemGetNInt( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_LLONG:
#if ! defined( HB_LONG_LONG_OFF )
         pArg->value.t.n64 = hb_itemGetNInt( pParam );
         if( pArg->bByRef )
            *r1 = ( HB_U32 ) &pArg->value.t.n64;
         else
         {
            *r1 = ( HB_U32 ) ( pArg->value.t.n64 & 0xFFFFFFFF );
            *r2 = ( HB_U32 ) ( pArg->value.t.n64 >> 32 );
            *b64 = HB_TRUE;
         }
#endif
         break;

      case HB_DYN_CTYPE_LLONG_UNSIGNED:
#if ! defined( HB_LONG_LONG_OFF )
         /* TOFIX: Digits are lost. */
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
         pArg->value.t.n64 = ( HB_MAXUINT ) hb_itemGetNInt( pParam );
#else
         pArg->value.t.n64 = ( HB_ULONGLONG ) hb_itemGetNInt( pParam );
#endif
         if( pArg->bByRef )
            *r1 = ( HB_U32 ) &pArg->value.t.n64;
         else
         {
            *r1 = ( HB_U32 ) ( pArg->value.t.n64 & 0xFFFFFFFF );
            *r2 = ( HB_U32 ) ( pArg->value.t.n64 >> 32 );
            *b64 = HB_TRUE;
         }
#endif
         break;

      case HB_DYN_CTYPE_FLOAT:
         pArg->value.t.nFL = ( float ) hb_itemGetND( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.nFL : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_DOUBLE:
         pArg->value.t.nDB = hb_itemGetND( pParam );
         if( pArg->bByRef )
            *r1 = ( HB_U32 ) &pArg->value.t.nDB;
         else
         {
#if ! defined( HB_LONG_LONG_OFF )
            *r1 = ( HB_U32 ) ( pArg->value.t.n64 & 0xFFFFFFFF );
            *r2 = ( HB_U32 ) ( pArg->value.t.n64 >> 32 );
            *b64 = HB_TRUE;
#endif
         }
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED_PTR:
      case HB_DYN_CTYPE_STRUCTURE:
      {
         HB_SIZE nLen = hb_itemGetCLen( pParam );
         pArg->hString = hb_xgrab( nLen + sizeof( char ) );
         pArg->bRawBuffer = HB_TRUE;
         memcpy( ( char * ) pArg->hString, hb_itemGetCPtr( pParam ), nLen );
         ( ( char * ) pArg->hString )[ nLen ] = '\0';
         *r1 = ( HB_PTRUINT ) pArg->hString;
         pArg->value.t.n32 = *r1;
         break;
      }
      case HB_DYN_CTYPE_CHAR_PTR:

         switch( pArg->iEncoding )
         {
            case HB_DYN_ENC_ASCII:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStr( pParam, hb_setGetOSCP(), &pArg->hString, &nLen );
               *r1 = ( HB_U32 ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_DYN_ENC_UTF8:
            {
               HB_SIZE nLen;
               const char * s = hb_itemGetStrUTF8( pParam, &pArg->hString, &nLen );
               *r1 = ( HB_U32 ) hb_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case HB_DYN_ENC_UTF16:
            {
               HB_SIZE nLen;
               const HB_WCHAR * s = hb_itemGetStrU16( pParam, HB_CDP_ENDIAN_NATIVE, &pArg->hString, &nLen );
               *r1 = ( HB_U32 ) hb_wstrunshare( &pArg->hString, s, nLen );
               break;
            }
            default:
            {
               HB_SIZE nLen = hb_itemGetCLen( pParam );
               pArg->hString = hb_xgrab( nLen + sizeof( char ) );
               pArg->bRawBuffer = HB_TRUE;
               memcpy( ( char * ) pArg->hString, hb_itemGetCPtr( pParam ), nLen );
               ( ( char * ) pArg->hString )[ nLen ] = '\0';
               *r1 = ( HB_PTRUINT ) pArg->hString;
               break;
            }
         }
         pArg->value.t.n32 = *r1;
         break;

      case HB_DYN_CTYPE_VOID_PTR:
      case HB_DYN_CTYPE_BOOL_PTR:
      case HB_DYN_CTYPE_SHORT_PTR:
      case HB_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_INT_PTR:
      case HB_DYN_CTYPE_INT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LONG_PTR:
      case HB_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LLONG_PTR:
      case HB_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_FLOAT_PTR:
      case HB_DYN_CTYPE_DOUBLE_PTR:
      case HB_DYN_CTYPE_STRUCTURE_PTR:
         pArg->value.t.n32 = ( HB_U32 ) hb_itemGetPtr( pParam );
         *r1 = pArg->bByRef ? ( HB_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case HB_DYN_CTYPE_VOID:
      default:
         *r1 = pArg->value.t.n32 = 0;
   }
}

static PHB_ITEM hb_u32ret( PHB_ITEM pItem, int iRetType, int iEncoding, HB_DYNVAL value, HB_ISIZ nLen )
{
   switch( iRetType )
   {
      case HB_DYN_CTYPE_VOID:
         hb_itemClear( pItem );
         break;

      case HB_DYN_CTYPE_BOOL:
         hb_itemPutL( pItem, value.t.n32 != 0 );
         break;

      case HB_DYN_CTYPE_CHAR:
         hb_itemPutNI( pItem, ( char ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED:
         hb_itemPutNI( pItem, ( unsigned char ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_SHORT:
         hb_itemPutNI( pItem, ( short ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_SHORT_UNSIGNED:
         hb_itemPutNI( pItem, ( unsigned short ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_INT:
         hb_itemPutNI( pItem, ( int ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_INT_UNSIGNED:
         hb_itemPutNInt( pItem, ( unsigned int ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_LONG:
         hb_itemPutNL( pItem, ( long ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_LONG_UNSIGNED:
         hb_itemPutNInt( pItem, value.t.n32 );
         break;

      case HB_DYN_CTYPE_LLONG:
#if ! defined( HB_LONG_LONG_OFF )
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
         hb_itemPutNInt( pItem, ( HB_MAXINT ) value.t.n64 );
#else
         hb_itemPutNInt( pItem, ( HB_LONGLONG ) value.t.n64 );
#endif
#endif
         break;

      case HB_DYN_CTYPE_LLONG_UNSIGNED:
#if ! defined( HB_LONG_LONG_OFF )
#if HB_VMLONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
         hb_itemPutNInt( pItem, ( HB_MAXUINT ) value.t.n64 );
#else
         hb_itemPutNInt( pItem, ( HB_ULONGLONG ) value.t.n64 );
#endif
#endif
         break;

      case HB_DYN_CTYPE_CHAR_UNSIGNED_PTR:
         if( nLen == -1 )
            hb_itemPutC( pItem, ( const char * ) value.t.n32 );
         else
            hb_itemPutCL( pItem, ( const char * ) value.t.n32, nLen );
         break;

      case HB_DYN_CTYPE_CHAR_PTR:

         switch( iEncoding )
         {
            case HB_DYN_ENC_ASCII:
               if( nLen == -1 )
                  hb_itemPutStr( pItem, hb_setGetOSCP(), ( const char * ) value.t.n32 );
               else
                  hb_itemPutStrLen( pItem, hb_setGetOSCP(), ( const char * ) value.t.n32, nLen );
               break;
            case HB_DYN_ENC_UTF8:
               if( nLen == -1 )
                  hb_itemPutStrUTF8( pItem, ( const char * ) value.t.n32 );
               else
                  hb_itemPutStrLenUTF8( pItem, ( const char * ) value.t.n32, nLen );
               break;
            case HB_DYN_ENC_UTF16:
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

      case HB_DYN_CTYPE_VOID_PTR:
      case HB_DYN_CTYPE_BOOL_PTR:
      case HB_DYN_CTYPE_SHORT_PTR:
      case HB_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_INT_PTR:
      case HB_DYN_CTYPE_INT_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LONG_PTR:
      case HB_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_LLONG_PTR:
      case HB_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case HB_DYN_CTYPE_FLOAT_PTR:
      case HB_DYN_CTYPE_DOUBLE_PTR:
      case HB_DYN_CTYPE_STRUCTURE_PTR:
         hb_itemPutPtr( pItem, ( void * ) value.t.n32 );
         break;

      case HB_DYN_CTYPE_FLOAT:
         hb_itemPutND( pItem, value.t.nFL );
         break;

      case HB_DYN_CTYPE_DOUBLE:
         hb_itemPutND( pItem, value.t.nDB );
         break;

      default:
         hb_itemPutNL( pItem, ( long ) value.t.n32 );
   }

   return pItem;
}

#define HB_DYN_CTYPE_DECL( ret, abi, _type_ ) \
   typedef ret ( abi * _type_##P00 )( void ); \
   typedef ret ( abi * _type_##P01 )( HB_U32 ); \
   typedef ret ( abi * _type_##P02 )( HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P03 )( HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P04 )( HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P05 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P06 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P07 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P08 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P09 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P10 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P11 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P12 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P13 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P14 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P15 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P16 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P17 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P18 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P19 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P20 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P21 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P22 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P23 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P24 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P25 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P26 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P27 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P28 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P29 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 ); \
   typedef ret ( abi * _type_##P30 )( HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32, HB_U32 )

#define HB_DYN_FUN_CALL( pcount, _ret_, _type_ ) \
   do \
   { \
      switch( pcount ) \
      { \
         case  0: ret.t._ret_ = ( ( _type_##P00 ) * pFunction )( ); break; \
         case  1: ret.t._ret_ = ( ( _type_##P01 ) * pFunction )( rawpar[ 0 ] ); break; \
         case  2: ret.t._ret_ = ( ( _type_##P02 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break; \
         case  3: ret.t._ret_ = ( ( _type_##P03 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break; \
         case  4: ret.t._ret_ = ( ( _type_##P04 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break; \
         case  5: ret.t._ret_ = ( ( _type_##P05 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break; \
         case  6: ret.t._ret_ = ( ( _type_##P06 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break; \
         case  7: ret.t._ret_ = ( ( _type_##P07 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break; \
         case  8: ret.t._ret_ = ( ( _type_##P08 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break; \
         case  9: ret.t._ret_ = ( ( _type_##P09 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break; \
         case 10: ret.t._ret_ = ( ( _type_##P10 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break; \
         case 11: ret.t._ret_ = ( ( _type_##P11 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break; \
         case 12: ret.t._ret_ = ( ( _type_##P12 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break; \
         case 13: ret.t._ret_ = ( ( _type_##P13 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break; \
         case 14: ret.t._ret_ = ( ( _type_##P14 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break; \
         case 15: ret.t._ret_ = ( ( _type_##P15 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break; \
         case 16: ret.t._ret_ = ( ( _type_##P16 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break; \
         case 17: ret.t._ret_ = ( ( _type_##P17 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break; \
         case 18: ret.t._ret_ = ( ( _type_##P18 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break; \
         case 19: ret.t._ret_ = ( ( _type_##P19 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break; \
         case 20: ret.t._ret_ = ( ( _type_##P20 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break; \
         case 21: ret.t._ret_ = ( ( _type_##P21 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break; \
         case 22: ret.t._ret_ = ( ( _type_##P22 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break; \
         case 23: ret.t._ret_ = ( ( _type_##P23 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break; \
         case 24: ret.t._ret_ = ( ( _type_##P24 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break; \
         case 25: ret.t._ret_ = ( ( _type_##P25 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break; \
         case 26: ret.t._ret_ = ( ( _type_##P26 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break; \
         case 27: ret.t._ret_ = ( ( _type_##P27 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break; \
         case 28: ret.t._ret_ = ( ( _type_##P28 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break; \
         case 29: ret.t._ret_ = ( ( _type_##P29 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break; \
         case 30: ret.t._ret_ = ( ( _type_##P30 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break; \
      } \
   } while( 0 )


#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   #define HB_CDECL  _cdecl
#else
   #define HB_CDECL
#endif

HB_DYN_CTYPE_DECL( HB_U32, HB_CDECL, FX86_C32 );
#if ! defined( HB_LONG_LONG_OFF )
HB_DYN_CTYPE_DECL( HB_U64, HB_CDECL, FX86_C64 );
#endif
HB_DYN_CTYPE_DECL( double, HB_CDECL, FX86_CDB );
HB_DYN_CTYPE_DECL( float,  HB_CDECL, FX86_CFL );

#if defined( HB_OS_WIN )

HB_DYN_CTYPE_DECL( HB_U32, _stdcall, FX86_S32 );
HB_DYN_CTYPE_DECL( HB_U64, _stdcall, FX86_S64 );
HB_DYN_CTYPE_DECL( double, _stdcall, FX86_SDB );
HB_DYN_CTYPE_DECL( float,  _stdcall, FX86_SFL );

#endif

#if defined( HB_OS_OS2 )

HB_DYN_CTYPE_DECL( HB_U32, _System, FX86_O32 );
HB_DYN_CTYPE_DECL( HB_U64, _System, FX86_O64 );
HB_DYN_CTYPE_DECL( double, _System, FX86_ODB );
HB_DYN_CTYPE_DECL( float,  _System, FX86_OFL );

#endif

#endif

void hb_dynCall( int iFuncFlags, void * pFunctionRaw, int iParams, int iFirst, int * piArgFlags )
{
   PHB_DYNADDR pFunction = ( PHB_DYNADDR ) pFunctionRaw;

   if( ! pFunction )
      return;

#if defined( HB_ARCH_64BIT )
   {
      int iRetType  = iFuncFlags & _MASK_CTYPE;
      int iEncoding = iFuncFlags & _MASK_ENCODING;
      int iOptions  = iFuncFlags & _MASK_OPTIONS;

      iParams -= iFirst - 1;

      if( iParams <= _DYNEXEC_MAXPARAM )
      {
         int iRetTypeRaw;
         HB_DYNVAL ret;
         HB_DYNARG * pArg;
         int tmp;

         HB_U64 rawpar[ _DYNEXEC_MAXPARAM ];

         ret.t.n64 = 0;

         if( iRetType == HB_DYN_CTYPE_DOUBLE )
            iRetTypeRaw = _RETTYPERAW_DOUBLE;
         else if( iRetType == HB_DYN_CTYPE_FLOAT )
            iRetTypeRaw = _RETTYPERAW_FLOAT;
         else
            iRetTypeRaw = _RETTYPERAW_INT64;

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
               pArg[ tmp ].iOptions  = piArgFlags[ tmp ] & _MASK_OPTIONS;
            }
            else
            {
               pArg[ tmp ].iType     = HB_DYN_CTYPE_DEFAULT;
               pArg[ tmp ].iEncoding = iEncoding;
               pArg[ tmp ].iOptions  = iOptions;
            }

            if( pArg[ tmp ].iType == HB_DYN_CTYPE_DEFAULT )
               pArg[ tmp ].iType = hb_hbtoctype( HB_ITEM_TYPE( pParam ) );

            pArg[ tmp ].bByRef = HB_ISBYREF( iFirst + tmp );

            rawpar[ tmp ] = hb_u64par( pParam, &pArg[ tmp ] );
         }

         switch( iRetTypeRaw )
         {
            case _RETTYPERAW_INT64:
               HB_DYN_FUN_CALL( iParams, n64, FX64_64 );
               break;
            case _RETTYPERAW_DOUBLE:
               HB_DYN_FUN_CALL( iParams, nDB, FX64_DB );
               break;
            case _RETTYPERAW_FLOAT:
               HB_DYN_FUN_CALL( iParams, nFL, FX64_FL );
               break;
         }

         hb_u64ret( hb_stackReturnItem(), iRetType, iEncoding, ret, -1 );

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            if( pArg[ tmp ].bByRef )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemParamStoreForward( ( HB_USHORT ) ( iFirst + tmp ),
                  hb_u64ret( pItem, pArg[ tmp ].iType, pArg[ tmp ].iEncoding, pArg[ tmp ].value,
                     ( pArg[ tmp ].iOptions & HB_DYC_OPT_NULLTERM ) != 0 ? -1 : ( HB_ISIZ ) hb_parclen( iFirst + tmp ) ) );

               hb_itemRelease( pItem );
            }

            if( pArg[ tmp ].bRawBuffer )
               hb_xfree( pArg[ tmp ].hString );
            else
               hb_strfree( pArg[ tmp ].hString );
         }

         if( pArg )
            hb_xfree( pArg );
      }
      else
         hb_errRT_BASE( EG_LIMIT, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
#elif defined( HB_ARCH_32BIT )
   {
      int iCallConv = iFuncFlags & _MASK_CALLCONV;
      int iRetType  = iFuncFlags & _MASK_CTYPE;
      int iEncoding = iFuncFlags & _MASK_ENCODING;
      int iOptions  = iFuncFlags & _MASK_OPTIONS;

      iParams -= iFirst - 1;

      if( iParams <= _DYNEXEC_MAXPARAM )
      {
         int iRetTypeRaw;
         HB_DYNVAL ret;
         HB_DYNARG * pArg;
         int tmp;

         int iParamsRaw = 0;
         HB_U32 rawpar[ _DYNEXEC_MAXPARAM * 2 ];

#if ! defined( HB_LONG_LONG_OFF )
         ret.t.n64 = 0;
#else
         memset( &ret, 0, sizeof( ret ) );
#endif

         if( iRetType == HB_DYN_CTYPE_DOUBLE )
            iRetTypeRaw = _RETTYPERAW_DOUBLE;
         else if( iRetType == HB_DYN_CTYPE_FLOAT )
            iRetTypeRaw = _RETTYPERAW_FLOAT;
         else if( iRetType == HB_DYN_CTYPE_LLONG ||
                  iRetType == HB_DYN_CTYPE_LLONG_UNSIGNED )
            iRetTypeRaw = _RETTYPERAW_INT64;
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
               pArg[ tmp ].iOptions  = piArgFlags[ tmp ] & _MASK_OPTIONS;
            }
            else
            {
               pArg[ tmp ].iType     = HB_DYN_CTYPE_DEFAULT;
               pArg[ tmp ].iEncoding = iEncoding;
               pArg[ tmp ].iOptions  = iOptions;
            }

            if( pArg[ tmp ].iType == HB_DYN_CTYPE_DEFAULT )
               pArg[ tmp ].iType = hb_hbtoctype( HB_ITEM_TYPE( pParam ) );

            pArg[ tmp ].bByRef = HB_ISBYREF( iFirst + tmp );

            hb_u32par( pParam, &pArg[ tmp ], &r1, &r2, &b64 );

            rawpar[ iParamsRaw++ ] = r1;
            if( b64 )
               rawpar[ iParamsRaw++ ] = r2;
         }

         switch( iCallConv )
         {
#if defined( HB_OS_WIN )
            case HB_DYN_CALLCONV_STDCALL:
               switch( iRetTypeRaw )
               {
                  case _RETTYPERAW_INT32:
                     HB_DYN_FUN_CALL( iParamsRaw, n32, FX86_S32 );
                     break;
                  case _RETTYPERAW_INT64:
                     HB_DYN_FUN_CALL( iParamsRaw, n64, FX86_S64 );
                     break;
                  case _RETTYPERAW_DOUBLE:
                     HB_DYN_FUN_CALL( iParamsRaw, nDB, FX86_SDB );
                     break;
                  case _RETTYPERAW_FLOAT:
                     HB_DYN_FUN_CALL( iParamsRaw, nFL, FX86_SFL );
                     break;
               }
               break;
#endif
#if defined( HB_OS_OS2 )
            case HB_DYN_CALLCONV_SYSCALL:
               switch( iRetTypeRaw )
               {
                  case _RETTYPERAW_INT32:
                     HB_DYN_FUN_CALL( iParamsRaw, n32, FX86_O32 );
                     break;
                  case _RETTYPERAW_INT64:
                     HB_DYN_FUN_CALL( iParamsRaw, n64, FX86_O64 );
                     break;
                  case _RETTYPERAW_DOUBLE:
                     HB_DYN_FUN_CALL( iParamsRaw, nDB, FX86_ODB );
                     break;
                  case _RETTYPERAW_FLOAT:
                     HB_DYN_FUN_CALL( iParamsRaw, nFL, FX86_OFL );
                     break;
               }
               break;
#endif
            default: /* HB_DYN_CALLCONV_CDECL */
               switch( iRetTypeRaw )
               {
                  case _RETTYPERAW_INT32:
                     HB_DYN_FUN_CALL( iParamsRaw, n32, FX86_C32 );
                     break;
                  case _RETTYPERAW_INT64:
#if ! defined( HB_LONG_LONG_OFF )
                     HB_DYN_FUN_CALL( iParamsRaw, n64, FX86_C64 );
#endif
                     break;
                  case _RETTYPERAW_DOUBLE:
                     HB_DYN_FUN_CALL( iParamsRaw, nDB, FX86_CDB );
                     break;
                  case _RETTYPERAW_FLOAT:
                     HB_DYN_FUN_CALL( iParamsRaw, nFL, FX86_CFL );
                     break;
               }
               break;
         }

         hb_u32ret( hb_stackReturnItem(), iRetType, iEncoding, ret, -1 );

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            if( pArg[ tmp ].bByRef )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemParamStoreForward( ( HB_USHORT ) ( iFirst + tmp ),
                  hb_u32ret( pItem, pArg[ tmp ].iType, pArg[ tmp ].iEncoding, pArg[ tmp ].value,
                     ( pArg[ tmp ].iOptions & HB_DYC_OPT_NULLTERM ) != 0 ? -1 : ( HB_ISIZ ) hb_parclen( iFirst + tmp ) ) );

               hb_itemRelease( pItem );
            }

            if( pArg[ tmp ].bRawBuffer )
               hb_xfree( pArg[ tmp ].hString );
            else
               hb_strfree( pArg[ tmp ].hString );
         }

         if( pArg )
            hb_xfree( pArg );
      }
      else
         hb_errRT_BASE( EG_LIMIT, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
#else
   HB_SYMBOL_UNUSED( iFuncFlags );
   HB_SYMBOL_UNUSED( iParams );
   HB_SYMBOL_UNUSED( iFirst );
   HB_SYMBOL_UNUSED( piArgFlags );
#endif
}
