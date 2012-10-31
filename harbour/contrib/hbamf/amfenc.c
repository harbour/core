/*
 * $Id$
 */

/*******
 *
 *  Ilina Stoilkovska <anili100/at/gmail.com> 2011
 *  Aleksander Czajczynski <hb/at/fki.pl> 2011-2012
 *
 *  Encoding Harbour items to AMF3
 *
 *  Contains portions from
 *  Dave Thompson's MIT licensed
 *  AmFast C library for Python
 *
 ********/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbapicls.h" /* for hb_objSetClass() */
#include "hbstack.h"

#include "hbapirdd.h"   /* for AMF3_FROMWA() */
#include "hbapierr.h"   /* as above */

#include "amf.h"

#include "hbdate.h"
#include "hbmath.h"

#include "hbvm.h"

typedef struct
{
   char *   cBuf;
   HB_ISIZ  position;
   HB_ISIZ  length;
   HB_BOOL  use_refs;
   HB_BOOL  use_strstr;
   HB_BOOL  str_rtrim;
   HB_SIZE  strstr_count; /* used only when str_ref is disabled */
   PHB_ITEM obj_ref;
   PHB_ITEM str_ref;
   PHB_ITEM strstr_ref;
   PHB_ITEM class_ref;
   PHB_ITEM conv_function;
   HB_BOOL  encode_ba;
} amfContext;

static HB_BOOL amf3_encode( amfContext * context, PHB_ITEM pItem );
extern HB_BOOL hbamf_is_cls_externalizable( HB_USHORT uiClass );

static void _ref_realItemPtr( PHB_ITEM pKey, PHB_ITEM pItem )
{
   if( HB_IS_STRING( pItem ) )
   {
      hb_itemPutPtr( pKey, ( void * ) hb_itemGetCPtr( pItem ) );
   }
   else if( HB_IS_ARRAY( pItem ) )
   {
      hb_itemPutPtr( pKey, hb_arrayId( pItem ) );
   }
   else if( HB_IS_HASH( pItem ) )
   {
      hb_itemPutPtr( pKey, hb_hashId( pItem ) );
   }
   else if( HB_IS_DATETIME( pItem ) )
   {
      hb_itemCopy( pKey, pItem );
   }
}

static HB_ISIZ bufferGrow( amfContext * context, HB_ISIZ len )
{
   HB_ISIZ new_len     = context->position + len;
   HB_ISIZ current_len = context->length;

   while( new_len > current_len )
      current_len *= 2;

   if( current_len != context->length )
   {
      context->cBuf = ( char * ) hb_xrealloc( context->cBuf, sizeof( char ) * current_len );
      if( ! context->cBuf )
      {
         return -1;
      }
      context->length = current_len;
   }
   return current_len;
}

static int writeByte( amfContext * context, char byte )
{
   if( bufferGrow( context, 1 ) == -1 )
      return 0;

   memcpy( context->cBuf + context->position, &byte, 1 );
   context->position += 1;
   return 1;
}

static HB_ISIZ writeBuffer( amfContext * context, const char * str, HB_ISIZ len )
{
   if( bufferGrow( context, len ) == -1 )
      return 0;

   memcpy( context->cBuf + context->position, str, len );
   context->position += len;
   return len;
}

static HB_BOOL amfX_encode_double( amfContext * context, double value )
{
   char   c_value[ 8 ];
   char * c_value_ref = &c_value[ 0 ];

   #ifndef HB_BIG_ENDIAN
   char flipped[ 8 ];
   #endif

   memcpy( c_value_ref, &value, 8 );

   #ifdef HB_BIG_ENDIAN
   if( writeBuffer( context, c_value_ref, 8 ) == 0 )
      return HB_FALSE;
   #else
   flipped[ 0 ] = c_value[ 7 ];
   flipped[ 1 ] = c_value[ 6 ];
   flipped[ 2 ] = c_value[ 5 ];
   flipped[ 3 ] = c_value[ 4 ];
   flipped[ 4 ] = c_value[ 3 ];
   flipped[ 5 ] = c_value[ 2 ];
   flipped[ 6 ] = c_value[ 1 ];
   flipped[ 7 ] = c_value[ 0 ];
   if( writeBuffer( context, flipped, 8 ) == 0 )
      return HB_FALSE;
   #endif

   return HB_TRUE;
}

static HB_BOOL amfX_write_double( amfContext * context, PHB_ITEM pItem )
{
   double d = hb_itemGetND( pItem );

   if( ! writeByte( context, DOUBLE_TYPE ) )
      return HB_FALSE;
   return amfX_encode_double( context, d );
}

static HB_BOOL amf3_encode_int( amfContext * context, int value )
{
   char    tmp[ 4 ];
   HB_SIZE tmp_size;

   /*
    * Int can be up to 4 bytes long.
    *
    * The first bit of the first 3 bytes
    * is set if another byte follows.
    *
    * The integer value is the last 7 bits from
    * the first 3 bytes and the 8 bits of the last byte
    * (29 bits).
    *
    * The int is negative if the 1st bit of the 29 int is set.
    */
   value &= 0x1fffffff; /* Ignore 1st 3 bits of 32 bit int, since we're encoding to 29 bit. */
   if( value < 0x80 )
   {
      tmp_size = 1;
      tmp[ 0 ] = ( char ) value;  /* TODO: some explicit casts in here to keep the compiler silent */
   }
   else if( value < 0x4000 )
   {
      tmp_size = 2;
      tmp[ 0 ] = ( char ) ( ( value >> 7 ) & 0x7f ) | 0x80;  /* Shift bits by 7 to fill 1st byte and set next byte flag */
      tmp[ 1 ] = ( char ) value & 0x7f;                      /* Shift bits by 7 to fill 2nd byte, leave next byte flag unset */
   }
   else if( value < 0x200000 )
   {
      tmp_size = 3;
      tmp[ 0 ] = ( char ) ( ( value >> 14 ) & 0x7f ) | 0x80;
      tmp[ 1 ] = ( char ) ( ( value >> 7 ) & 0x7f ) | 0x80;
      tmp[ 2 ] = ( char ) value & 0x7f;
   }
   else if( value < 0x40000000 )
   {
      tmp_size = 4;
      tmp[ 0 ] = ( char ) ( ( value >> 22 ) & 0x7f ) | 0x80;
      tmp[ 1 ] = ( char ) ( ( value >> 15 ) & 0x7f ) | 0x80;
      tmp[ 2 ] = ( char ) ( ( value >> 8 ) & 0x7f ) | 0x80; /* Shift bits by 8, since we can use all bits in the 4th byte */
      tmp[ 3 ] = ( char ) ( value & 0xff );
   }
   else
   {
      return HB_FALSE;
   }

   if( ( HB_SIZE ) writeBuffer( context, tmp, tmp_size ) != tmp_size )
      return HB_FALSE;

   return HB_TRUE;
}

static HB_BOOL amf3_write_int( amfContext * context, PHB_ITEM pItem )
{
#ifndef HB_LONG_LONG_OFF
   HB_LONGLONG n = hb_itemGetNLL( pItem );
#else
   long n = hb_itemGetNL( pItem );
#endif

   if( n > MIN_INT && n < MAX_INT )
   {
      if( ! writeByte( context, INT_TYPE ) )
         return HB_FALSE;
      return amf3_encode_int( context, ( int ) n );
   }
   else
   {
      if( ! writeByte( context, DOUBLE_TYPE ) )
         return HB_FALSE;
      return amfX_encode_double( context, ( double ) n );
   }
}

/*
   static HB_BOOL amf3_encode_float(amfContext * context, PHB_ITEM pItem)
   {
   float n = (float)hb_itemGetND(pItem);
   return amfX_encode_double(context, (double)n);
   }
 */

static HB_BOOL amf3_encode_nil( amfContext * context )
{
   return writeByte( context, NULL_TYPE );
}

static HB_BOOL amf3_encode_bool( amfContext * context, PHB_ITEM pItem )
{
   if( hb_itemGetL( pItem ) )
      return writeByte( context, TRUE_TYPE );
   else
      return writeByte( context, FALSE_TYPE );
}

static HB_BOOL amf3_encode_string( amfContext * context, PHB_ITEM pItem )
{
   void *       hStr = NULL;       /* = hb_itemGetCPtr(pItem); not needed with UTF8 conversion */
   HB_SIZE      len;
   const char * utf8str = hb_itemGetStrUTF8( pItem, &hStr, &len );
   HB_BOOL      result;

   if( ! hStr )
      return HB_FALSE;

   if( context->str_rtrim )
      len = hb_strRTrimLen( utf8str, len, HB_FALSE );

   if( ! amf3_encode_int( context, ( ( int ) len << 1 ) | REFERENCE_BIT ) )
      return HB_FALSE;

   result = ( writeBuffer( context, utf8str, len ) != 0 );

   hb_strfree( hStr );

   return result;
}

static int amf3_add_index( amfContext * context, PHB_ITEM pHash, PHB_ITEM pItem )
{
   PHB_ITEM pKey;
   PHB_ITEM pVal;
   int      result = 0;
   HB_SIZE  str_len;

   if( context->use_refs )
   {
      pKey = hb_itemNew( NULL );
      _ref_realItemPtr( pKey, pItem );
      if( ! HB_IS_POINTER( pKey ) && ! HB_IS_DATETIME( pKey ) )
      {
         hb_itemRelease( pKey );
         return -1;
      }

      pVal = hb_itemNew( NULL );

      if( ! hb_hashAdd( pHash, pKey, pVal ) )
      {
         hb_itemRelease( pKey );
         hb_itemRelease( pVal );
         return -1;
      }
      hb_itemRelease( pVal );

      hb_itemRelease( pKey );
      result = ( int ) ( hb_hashLen( pHash ) - 1 + context->strstr_count );
      /* used only when some inner context inside
       * conversion function uses only strstr mode
       * like AMF3_FROMWA function f.e. */
   }

   if( ( HB_IS_STRING( pItem ) || HB_IS_MEMO( pItem ) ) && context->use_strstr )
   {
      str_len = hb_itemGetCLen( pItem );
      if( str_len > 3 && str_len < 32 ) /* do this only for mid-sized strings */
      {
         if( ! context->use_refs )
            result = ( int ) context->strstr_count;

         pVal = hb_itemPutNS( NULL, result ); /* put the AMF reference id as value */
         hb_hashAdd( context->strstr_ref, pItem, pVal );
         hb_itemRelease( pVal );
      }
      if( ! context->use_refs )
         context->strstr_count++;
   }

   return result;
}

static int amf3_get_index( amfContext * context, PHB_ITEM pHash, PHB_ITEM pItem )
{
   PHB_ITEM pKey;
   PHB_ITEM pStrIdx;
   HB_SIZE  nPos;
   HB_SIZE  str_len;

   if( context->use_refs )
   {
      pKey = hb_itemNew( NULL );
      _ref_realItemPtr( pKey, pItem );
      if( ! HB_IS_POINTER( pKey ) && ! HB_IS_DOUBLE( pKey ) )
      {
         hb_itemRelease( pKey );
         return -1;
      }
      if( hb_hashScan( pHash, pKey, &nPos ) )
      {
         hb_itemRelease( pKey );
         return ( int ) ( nPos - 1 );
      }
      else
         hb_itemRelease( pKey );
   }

   if( ( HB_IS_STRING( pItem ) || HB_IS_MEMO( pItem ) ) && context->use_strstr )
   {
      str_len = hb_itemGetCLen( pItem );
      if( str_len > 3 && str_len < 32 ) /* do this only for mid-sized strings */
      {
         pStrIdx = hb_hashGetItemPtr( context->strstr_ref, pItem, 0 );
         if( pStrIdx )
            return ( int ) hb_itemGetNS( pStrIdx );
         else
            return -1;
      }
      else
         return -1;
   }

   return -1;
}

static int amf3_encode_reference( amfContext * context, PHB_ITEM pHash, PHB_ITEM pItem, int bit )
{
   int idx;

   if( pItem == NULL )
      return -1;

   idx = amf3_get_index( context, pHash, pItem );
   if( idx > -1 )
   {
      if( idx < MAX_INT )
      {
         if( ! amf3_encode_int( context, ( idx << ( bit + 1 ) ) | ( 0x00 + bit ) ) )
            return 0;
         return 1;
      }
   }

   if( amf3_add_index( context, pHash, pItem ) == -1 )
      return 0;


   return -1;
}

static HB_BOOL amf3_serialize_string( amfContext * context, PHB_ITEM pItem )
{
   int     result;
   HB_SIZE len = hb_itemGetCLen( pItem );

   if( len == 0 )
      return writeByte( context, EMPTY_STRING_TYPE );
   else if( context->str_rtrim && hb_strRTrimLen( hb_itemGetCPtr( pItem ), len, HB_FALSE ) == 0 )
      return writeByte( context, EMPTY_STRING_TYPE );

   result = amf3_encode_reference( context, context->str_ref, pItem, 0 );
   if( result > -1 )
      return result;

   return amf3_encode_string( context, pItem );
}

/*
   static HB_BOOL amf3_serialize_object_as_string(amfContext * context, PHB_ITEM pItem)
   {
   PHB_ITEM pStr;
   HB_BOOL result;

   if( HB_IS_STRING(pItem) || HB_IS_MEMO( pItem ) )
      return amf3_serialize_string(context, pItem);

   if(!hb_itemPutC(pStr, hb_itemGetCPtr(pItem)))
      return HB_FALSE;

   result = amf3_serialize_string(context, pStr);

   hb_itemRelease(pStr);
   return result;
   }

 */

static HB_BOOL amf3_encode_hash( amfContext * context, PHB_ITEM pItem )
{
   PHB_ITEM pKey;
   PHB_ITEM pVal;
   HB_ISIZ  i;
   HB_ISIZ  len      = hb_hashLen( pItem );
   HB_ISIZ  nIntKeys = 0;

   if( ( ( ( hb_hashGetFlags( pItem ) & HB_HASH_KEEPORDER ) != 0
           && HB_IS_INTEGER( hb_hashGetKeyAt( pItem, 1 ) ) )
         || hb_hashGetFlags( pItem ) & HB_HASH_KEEPORDER ) == 0 )
   {
      for( i = 1; i <= len; i++ )
      {
         pKey = hb_hashGetKeyAt( pItem, i );
         if( HB_IS_INTEGER( pKey ) )
            nIntKeys++;
      }
   }

   if( ! amf3_encode_int( context, ( int ) ( ( nIntKeys << 1 ) | REFERENCE_BIT ) ) )
      return HB_FALSE;

   for( i = 1; i <= len; i++ )
   {
      pKey = hb_hashGetKeyAt( pItem, i );
      pVal = hb_hashGetValueAt( pItem, i );
      if( HB_IS_STRING( pKey ) )
      {
         if( ! amf3_encode_string( context, pKey ) )
            return HB_FALSE;
         if( ! amf3_encode( context, pVal ) )
            return HB_FALSE;
      }
   }

   if( ! writeByte( context, EMPTY_STRING_TYPE ) )
      return HB_FALSE;

   if( nIntKeys > 0 )
   {
      for( i = 1; i <= len; i++ )
      {
         pKey = hb_hashGetKeyAt( pItem, i );
         pVal = hb_hashGetValueAt( pItem, i );
         if( HB_IS_INTEGER( pKey ) )
         {
            if( ! amf3_encode( context, pVal ) )
               return HB_FALSE;
         }
      }
   }

   return HB_TRUE;
}

static HB_BOOL amf3_encode_dynamic_dict( amfContext * context, PHB_ITEM pItem )
{
   PHB_ITEM pKey;
   PHB_ITEM pVal;
   HB_ISIZ  i;
   HB_ISIZ  len = hb_hashLen( pItem );

   for( i = 1; i <= len; i++ )
   {
      pKey = hb_hashGetKeyAt( pItem, i );
      pVal = hb_hashGetValueAt( pItem, i );
      if( HB_IS_STRING( pKey ) )
      {
         if( ! amf3_serialize_string( context, pKey ) )
            return HB_FALSE;
         if( ! amf3_encode( context, pVal ) )
            return HB_FALSE;
      }
   }

   if( ! writeByte( context, EMPTY_STRING_TYPE ) )
      return HB_FALSE;

   return HB_TRUE;
}

static HB_BOOL amf3_serialize_hash( amfContext * context, PHB_ITEM pItem )
{
   HB_BOOL result = amf3_encode_reference( context, context->obj_ref, pItem, 0 );

   if( result > -1 )
   {
      return result;
   }

   return amf3_encode_hash( context, pItem );
}

static HB_BOOL amf3_write_hash( amfContext * context, PHB_ITEM pItem )
{
   if( ! writeByte( context, ARRAY_TYPE ) )
      return 0;

   return amf3_serialize_hash( context, pItem );
}

static HB_ISIZ amf3_encode_byte_array( amfContext * context, PHB_ITEM pItem )
{
   HB_ISIZ      item_len;
   const char * bytes;

   if( HB_IS_STRING( pItem ) || HB_IS_MEMO( pItem ) )
   {
      item_len = hb_itemGetCLen( pItem );
      bytes    = hb_itemGetCPtr( pItem );
   }
   else
      return HB_FALSE;

   if( ! bytes )
      return HB_FALSE;

   return writeBuffer( context, bytes, item_len );
}

static HB_ISIZ amf3_serialize_byte_array( amfContext * context, PHB_ITEM pItem )
{
   int result;

   if( hb_itemGetCLen( pItem ) == 0 )
      return writeByte( context, EMPTY_STRING_TYPE );

   result = amf3_encode_reference( context, context->obj_ref, pItem, 0 );
   if( result > -1 )
      return result;

   if( ! amf3_encode_int( context, ( ( int ) hb_itemGetCLen( pItem ) << 1 ) | REFERENCE_BIT ) )
      return HB_FALSE;

   return amf3_encode_byte_array( context, pItem );
}

static int amf3_encode_date( amfContext * context, PHB_ITEM pItem )
{
   double timestamp;

   if( ! amf3_encode_int( context, REFERENCE_BIT ) )
      return 0;

   timestamp = ( ( hb_itemGetTD( pItem ) - 2440587.5 ) * 86400000 );

   return amfX_encode_double( context, timestamp );
}

static int amf3_serialize_date( amfContext * context, PHB_ITEM pItem )
{
   int result = amf3_encode_reference( context, context->obj_ref, pItem, 0 );

   if( result > -1 )
      return result;

   return amf3_encode_date( context, pItem );
}

static HB_BOOL amf3_encode_array( amfContext * context, PHB_ITEM pItem )
{
   HB_SIZE  item_len = hb_arrayLen( pItem );
   PHB_ITEM pArrayItem;
   int      i;
   int      result;

   if( ! amf3_encode_int( context, ( ( int ) item_len << 1 ) | REFERENCE_BIT ) )
      return HB_FALSE;

   if( ! writeByte( context, NULL_TYPE ) )
      return HB_FALSE;

   for( i = 1; i <= ( int ) item_len; i++ )
   {
      pArrayItem = hb_itemNew( NULL );
      hb_arrayGet( pItem, i, pArrayItem );
      if( ! pArrayItem )
         return HB_FALSE;

      result = amf3_encode( context, pArrayItem );
      hb_itemRelease( pArrayItem );
      if( ! result )
         return HB_FALSE;
   }

   return HB_TRUE;
}

static HB_BOOL amf3_serialize_array( amfContext * context, PHB_ITEM pItem )
{
   int result = amf3_encode_reference( context, context->obj_ref, pItem, 0 );

   if( result > -1 )
      return result;
   return amf3_encode_array( context, pItem );
}

static int amf3_encode_class_def( amfContext * context, PHB_ITEM pClass )
{
   int      header;
   int      result;
   HB_ISIZ  static_attr_len;
   HB_ISIZ  i;
   PHB_ITEM class_alias;
   PHB_ITEM static_attrs;
/* PHB_ITEM attr_len = NULL; */
   PHB_ITEM attr_name;

   if( ! pClass )
   {
      if( ! writeByte( context, DYNAMIC ) )
         return 0;

      if( ! writeByte( context, EMPTY_STRING_TYPE ) )
         return 0;
      return 1;
   }

   if( hb_hashGetCItemPos( pClass, "CLASS_DEF" ) == 0 )
   {
      return 0;
   }

   if( hb_hashGetCItemPos( pClass, "EXTERNALIZABLE_CLASS_DEF" ) != 0 )
   {
      header = EXTERNALIZABLE;
   }
   else if( hb_hashGetCItemPos( pClass, "DYNAMIC_CLASS_DEF" ) != 0 )
   {
      header = DYNAMIC;
   }
   else
   {
      header = STATIC;
   }

   class_alias = hb_hashGetCItemPtr( pClass, "alias" );
   if( ! class_alias )
      return 0;

   if( header == EXTERNALIZABLE )
   {
      if( ! amf3_encode_int( context, header ) )
      {
         return 0;
      }
      result = amf3_serialize_string( context, class_alias );
      return result;
   }

   static_attrs = hb_hashGetCItemPtr( pClass, "static_attrs" );
   if( ! static_attrs )
   {
      return 0;
   }

   static_attr_len = hb_arrayLen( static_attrs ); /* array this is -- hb_itemGetCLen(static_attrs); */
   if( static_attr_len == -1 || static_attr_len > ( MAX_INT >> 4 ) )
   {
      return 0;
   }

   header |= ( ( int ) static_attr_len ) << 4;
   if( ! amf3_encode_int( context, header ) )
   {
      return 0;
   }

   result = amf3_serialize_string( context, class_alias );
   /* not needed  hb_itemRelease(class_alias); */
   if( ! result )
   {
      return 0;
   }

   for( i = 0; i < static_attr_len; i++ )
   {
      attr_name = hb_itemArrayGet( static_attrs, i );
      if( ! attr_name )
      {
         /* not needed hb_itemRelease(static_attrs); */
         return 0;
      }
      result = amf3_serialize_string( context, attr_name );
      hb_itemRelease( attr_name );
      if( ! result )
         return 0;
   }

   /* not needed  hb_itemRelease( static_attrs ); */
   return 1;
}

static int amf3_serialize_class_def( amfContext * context, PHB_ITEM pClass )
{
   int result = amf3_encode_reference( context, context->class_ref, pClass, 0 );

   if( result > -1 )
      return result;

   return amf3_encode_class_def( context, pClass );
}

/* Get an object's class def. */
static PHB_ITEM class_def_from_class( /* amfContext * context, */ PHB_ITEM pItem )
{
   HB_USHORT uiClass;
   PHB_ITEM  pClass;
   PHB_ITEM  pKey;
   PHB_ITEM  pValue;

   /* get Harbour's class id/handle */
   uiClass = hb_objGetClass( pItem );
   if( ! uiClass )
      return NULL;

   /* we left this (python-originated) indirect method of storing
      class "properties" in a hash, this it may be easier in the
      future to implement some additional class mapper instead of
      "tags" which are put into the Harbour class definitions
      right now */

   pClass = hb_hashNew( NULL );

   pKey   = hb_itemPutC( NULL, "CLASS_DEF" );
   pValue = hb_itemNew( NULL );
   if( ! hb_hashAdd( pClass, pKey, pValue ) )
   {
      hb_itemRelease( pKey );
      hb_itemRelease( pValue );
      hb_itemRelease( pClass );
      return NULL;
   }
   hb_itemRelease( pKey );
   hb_itemRelease( pValue );

   pKey   = hb_itemPutC( NULL, "alias" );
   pValue = hb_itemPutC( NULL, hb_clsName( uiClass ) );
   if( ! hb_hashAdd( pClass, pKey, pValue ) )
   {
      hb_itemRelease( pKey );
      hb_itemRelease( pValue );
      hb_itemRelease( pClass );
      return NULL;
   }
   hb_itemRelease( pKey );
   hb_itemRelease( pValue );

   if( hbamf_is_cls_externalizable( uiClass ) )
   {
      pKey   = hb_itemPutC( NULL, "EXTERNALIZABLE_CLASS_DEF" );
      pValue = hb_itemNew( NULL );
      if( ! hb_hashAdd( pClass, pKey, pValue ) )
      {
         hb_itemRelease( pKey );
         hb_itemRelease( pValue );
         hb_itemRelease( pClass );
         return NULL;
      }
      hb_itemRelease( pKey );
      hb_itemRelease( pValue );
   }

/* if we ever want to store the objects dynamically
   hb_itemPutC( pKey, "DYNAMIC_CLASS_DEF" );
   hb_itemNew( pValue );
   if( !hb_hashAdd( pClass, pKey, pValue ) )
   {
      hb_itemRelease( pKey );
      hb_itemRelease( pValue );
      hb_itemRelease( pClass );
      return NULL;
   }
 */

   return pClass;
}

static HB_BOOL amf3_encode_object( amfContext * context, PHB_ITEM pItem )
{
   HB_BOOL  result;
   PHB_ITEM pClass;

   /* serialize emulated ActionScript dynamic object */
   if( strcmp( hb_clsName( hb_objGetClass( pItem ) ), "AMF_OBJ" ) == 0 )
   {
      PHB_ITEM pAnonHash = hb_itemNew( NULL );

      if( amf3_serialize_class_def( context, NULL ) == 0 )
      {
         hb_itemRelease( pAnonHash );
         return HB_FALSE;
      }

      hb_arrayGet( pItem, OBJAMF_VAR_HASH, pAnonHash );
      result = amf3_encode_dynamic_dict( context, pAnonHash );
      hb_itemRelease( pAnonHash );
      return result;
   }

   pClass = class_def_from_class( /* context, */ pItem );
   if( ! pClass )
      return HB_FALSE;

   if( ! amf3_serialize_class_def( context, pClass ) )
   {
      hb_itemRelease( pClass );
      return HB_FALSE;
   }

   if( hb_hashGetCItemPos( pClass, "EXTERNALIZABLE_CLASS_DEF" ) != 0 )
   {
      PHB_ITEM pStr;
      PHB_ITEM pRetCopy = hb_itemNew( NULL );
      PHB_ITEM pObject;

      if( pItem == hb_stackReturnItem() )
      {
         pObject = pRetCopy;
      }
      else
      {
         pObject = pItem;
      }

      hb_itemMove( pRetCopy, hb_stackReturnItem() );

      pStr = hb_objSendMsg( pObject, "WRITEEXTERNAL", 0 );
      if( ! pStr )
      {
         hb_itemMove( hb_stackReturnItem(), pRetCopy );
         hb_itemRelease( pRetCopy );
         hb_itemRelease( pClass );
         return HB_FALSE;
      }

      result = amf3_encode_byte_array( context, pStr ) != 0;
      hb_itemRelease( pClass );

      hb_itemMove( hb_stackReturnItem(), pRetCopy );
      hb_itemRelease( pRetCopy );

      return result;
   }

   hb_itemRelease( pClass );
   return HB_FALSE;

#if 0
   /* Encoding attributes of class mapped objects is still a TODO, so the Python code is left for reference. */

   /* Encode static attrs */
   PyObject * static_attrs = static_attr_vals_from_class_def( context, class_def, value );
   if( ! static_attrs )
   {
      Py_DECREF( class_def );
      return 0;
   }

   Py_ssize_t static_attr_len = PySequence_Size( static_attrs );
   if( static_attr_len == -1 )
   {
      Py_DECREF( class_def );
      Py_DECREF( static_attrs );
      return 0;
   }

   int i;
   for( i = 0; i < static_attr_len; i++ )
   {
      PyObject * static_attr = PySequence_GetItem( static_attrs, i );
      if( ! static_attr )
      {
         Py_DECREF( static_attrs );
         Py_DECREF( class_def );
         return 0;
      }

      int result = encode_AMF3( context, static_attr );
      Py_DECREF( static_attr );
      if( ! result )
      {
         Py_DECREF( static_attrs );
         Py_DECREF( class_def );
         return 0;
      }
   }
   Py_DECREF( static_attrs );

   /* Encode dynamic attrs */
   if( PyObject_HasAttrString( class_def, "DYNAMIC_CLASS_DEF" ) )
   {
      PyObject * dynamic_attrs = dynamic_attrs_from_class_def( context, class_def, value );
      if( ! dynamic_attrs )
      {
         Py_DECREF( class_def );
         return 0;
      }

      int result = encode_dynamic_dict_AMF3( context, dynamic_attrs );
      Py_DECREF( dynamic_attrs );
      if( ! result )
      {
         Py_DECREF( class_def );
         return 0;
      }
   }

   Py_DECREF( class_def );
   return 1;

#endif
}

static HB_BOOL amf3_serialize_object( amfContext * context, PHB_ITEM pItem )
{
   int result;

   if( strcmp( hb_clsName( hb_objGetClass( pItem ) ), "AMF_RAW" ) == 0 )
   {
      PHB_ITEM pStr = hb_itemNew( NULL );
      hb_arrayGet( pItem, 1, pStr );
      context->position--;
      result = amf3_encode_byte_array( context, pStr ) != 0;
      hb_itemRelease( pStr );
      return result;
   }

   result = amf3_encode_reference( context, context->obj_ref, pItem, 0 );

   if( result > -1 )
      return result;

   return amf3_encode_object( context, pItem );
}

static void amf3_conversion_out( amfContext * context, PHB_ITEM pItem )
{
   PHB_ITEM pRetCopy      = hb_itemNew( NULL );
   PHB_ITEM pOuterContext = hb_itemPutPtr( NULL, context );
   PHB_SYMB pSym = hb_itemGetSymbol( context->conv_function );

   if( pItem == hb_stackReturnItem() )
   {
      hb_vmPushSymbol( pSym );
      hb_vmPushNil();
      hb_vmPush( pItem );
      hb_vmPush( pOuterContext );
      hb_vmDo( 2 );
   }
   else
   {
      hb_itemMove( pRetCopy, hb_stackReturnItem() );
      hb_vmPushSymbol( pSym );
      hb_vmPushNil();
      hb_vmPush( pItem );
      hb_vmPush( pOuterContext );
      hb_vmDo( 2 );
      hb_itemMove( pItem, hb_stackReturnItem() );
      hb_itemMove( hb_stackReturnItem(), pRetCopy );
   }
   hb_itemRelease( pOuterContext );
   hb_itemRelease( pRetCopy );
}

static HB_BOOL amf3_encode( amfContext * context, PHB_ITEM pItem )
{
   HB_BOOL result = HB_FALSE;

   if( context->conv_function )
      amf3_conversion_out( context, pItem );
   if( HB_IS_NIL( pItem ) )
   {
      result = amf3_encode_nil( context );
   }
   else if( HB_IS_LOGICAL( pItem ) )
   {
      result = amf3_encode_bool( context, pItem );
   }
   else if( HB_IS_INTEGER( pItem ) || HB_IS_LONG( pItem ) )
   {
      result = amf3_write_int( context, pItem );
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      result = amfX_write_double( context, pItem );
   }
   else if( HB_IS_STRING( pItem ) || HB_IS_MEMO( pItem ) )
   {
      if( context->encode_ba )
      {
         if( ! writeByte( context, BYTE_ARRAY_TYPE ) )
            result = HB_FALSE;
         else
            result = amf3_serialize_byte_array( context, pItem ) != 0;
      }
      else
      {
         if( ! writeByte( context, STRING_TYPE ) )
            result = HB_FALSE;
         else
            result = amf3_serialize_string( context, pItem );
      }
   }
   else if( HB_IS_DATETIME( pItem ) )
   {
      if( ! writeByte( context, DATE_TYPE ) )
         result = HB_FALSE;
      else
         result = amf3_serialize_date( context, pItem );
   }
   else if( HB_IS_OBJECT( pItem ) )
   {
      if( ! writeByte( context, OBJECT_TYPE ) )
         result = HB_FALSE;
      else
         result = amf3_serialize_object( context, pItem );
   }
   else if( HB_IS_ARRAY( pItem ) )
   {
      if( ! writeByte( context, ARRAY_TYPE ) )
         result = HB_FALSE;
      else
         result = amf3_serialize_array( context, pItem );
   }
   else if( HB_IS_HASH( pItem ) )
   {
      result = amf3_write_hash( context, pItem );
   }

   return result;
}

static amfContext * context_setup( PHB_ITEM pFuncSym, HB_BOOL use_refs, HB_BOOL str_rtrim, amfContext * outer_context )
{
   amfContext * context;

   context = ( amfContext * ) hb_xgrab( sizeof( amfContext ) );
   memset( context, 0, sizeof( amfContext ) );

   context->cBuf = ( char * ) hb_xgrab( sizeof( char ) * 8 );
   /* memset( context->cBuf, 0, sizeof( char ) * 8 ); */
   context->position  = 0;
   context->length    = sizeof( char ) * 8;
   context->str_rtrim = str_rtrim;
   context->use_refs  = use_refs;
   if( use_refs )
   {
      if( outer_context && outer_context->use_refs )
      {
         context->obj_ref   = outer_context->obj_ref;
         context->str_ref   = outer_context->str_ref;
         context->class_ref = outer_context->class_ref;
      }
      else
      {
         context->obj_ref   = hb_hashNew( NULL );
         context->str_ref   = hb_hashNew( NULL );
         context->class_ref = hb_hashNew( NULL );
         hb_hashSetFlags( context->obj_ref, HB_HASH_KEEPORDER );
         hb_hashSetFlags( context->str_ref, HB_HASH_KEEPORDER );
         hb_hashSetFlags( context->class_ref, HB_HASH_KEEPORDER );
      }
   }
   else
   {
      context->obj_ref   = NULL;
      context->str_ref   = NULL;
      context->class_ref = NULL;
   }

   context->conv_function = pFuncSym;

   /* "strstr" is another optional idea of catching similar strings,
      key in this hash is not the pointer to C char, but the string
      itself and the value is id of the reference */
   context->use_strstr = HB_TRUE;

   if( outer_context )
   {
      context->strstr_count = outer_context->strstr_count;
      if( outer_context->use_strstr )
         context->strstr_ref = outer_context->strstr_ref;
      else
         context->strstr_ref = hb_hashNew( NULL );

      if( ! context->use_refs && outer_context->use_refs )
         context->strstr_count += hb_hashLen( outer_context->str_ref );
   }
   else
   {
      context->strstr_count = 0;
      context->strstr_ref   = hb_hashNew( NULL );
   }

   return context;
}

static void context_release( amfContext * context, amfContext * outer_context )
{
   if( context->use_refs && ! ( outer_context && outer_context->use_refs ) )
   {
      hb_itemRelease( context->obj_ref );
      hb_itemRelease( context->str_ref );
      hb_itemRelease( context->class_ref );
   }

   if( context->use_strstr )
   {
      if( outer_context )
      {
         if( ! context->use_refs && outer_context->use_refs )
            context->strstr_count -= hb_hashLen( outer_context->str_ref );

         outer_context->strstr_count = context->strstr_count;

         if( ! outer_context->strstr_ref )
            hb_itemRelease( context->strstr_ref );
      }
      else
         hb_itemRelease( context->strstr_ref );
   }
}

HB_FUNC( AMF3_FROMWA )
{
   PHB_ITEM     pWhile        = hb_param( 1, HB_IT_BLOCK );
   PHB_ITEM     pFor          = hb_param( 2, HB_IT_BLOCK );
   PHB_ITEM     pFields       = hb_param( 3, HB_IT_ARRAY );
   HB_ULONG     nCount        = hb_parnldef( 4, 0 );
   HB_BOOL      str_rtrim     = hb_parldef( 5, HB_TRUE );
   HB_USHORT    nPkg          = ( HB_USHORT ) hb_parnidef( 6, 0 );
   amfContext * outer_context = ( amfContext * ) hb_parptr( 7 );

   DBORDERINFO  pInfo;
   int          iOrd;
   HB_USHORT    uiFields;
   HB_ULONG     uiRecCount     = 0;
   HB_ULONG     uiRecNo        = 0;
   HB_BOOL      bNoFieldPassed = ( pFields == NULL || hb_arrayLen( pFields ) == 0 );
   HB_BOOL      bEof  = HB_FALSE;
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM     pItem;
   HB_USHORT    uiFieldCopy = 0;      /* GCC knows better (warns) */
   HB_USHORT    uiIter;
   amfContext * context;
   HB_BOOL      bPredictLen = ( ! pWhile && ! pFor );

   HB_BOOL  bAsArray    = ! nPkg;
   PHB_ITEM pFieldNames = NULL;          /* again GCC knows better */
   PHB_ITEM pField;

   if( pArea )
   {
      memset( &pInfo, 0, sizeof( pInfo ) );
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );
      iOrd = hb_itemGetNI( pInfo.itmResult );
      if( iOrd > 0 )
      {
         SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &pInfo );
         uiRecCount = hb_itemGetNL( pInfo.itmResult );
         SELF_ORDINFO( pArea, DBOI_POSITION, &pInfo );
         uiRecNo = hb_itemGetNL( pInfo.itmResult );

         hb_itemRelease( pInfo.itmResult );
      }
      else
      {
         hb_itemRelease( pInfo.itmResult );

         if( SELF_RECCOUNT( pArea, &uiRecCount ) != HB_SUCCESS )
            return;

         if( SELF_RECNO( pArea, &uiRecNo ) != HB_SUCCESS )
            return;
      }

      pItem = hb_itemNew( NULL );

      context = context_setup( NULL, HB_FALSE, str_rtrim, outer_context );

      if( bPredictLen )
      {
         if( nCount == 0 || uiRecNo + nCount > uiRecCount )
            nCount = uiRecCount - uiRecNo + 1;
      }
      else
      {
         uiRecCount = 0;
         while( ( nCount == 0 || uiRecCount < nCount ) &&
                ( ! pWhile || hb_itemGetL( hb_vmEvalBlock( pWhile ) ) ) )
         {
            if( SELF_EOF( pArea, &bEof ) != HB_SUCCESS )
               break;

            if( bEof )
               break;

            if( ! pFor || hb_itemGetL( hb_vmEvalBlock( pFor ) ) )
               uiRecCount++;

            if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
               break;
         }
         nCount = uiRecCount;

         if( iOrd > 0 )
         {
            memset( &pInfo, 0, sizeof( pInfo ) );
            pInfo.itmNewVal = hb_itemPutNL( NULL, uiRecNo );
            pInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
            SELF_ORDINFO( pArea, DBOI_POSITION, &pInfo );
            hb_itemRelease( pInfo.itmNewVal );
            hb_itemRelease( pInfo.itmResult );
         }
         else
            SELF_GOTO( pArea, uiRecNo );
      }

      /* TODO: should be if( writeByte() ), before we make a variant that operates on streams directly */

      writeByte( context, ARRAY_TYPE );
      amf3_encode_int( context, ( ( int ) nCount << 1 ) | REFERENCE_BIT );
      writeByte( context, NULL_TYPE );

      SELF_FIELDCOUNT( pArea, &uiFields );

      if( ! bNoFieldPassed )
      {
         uiFieldCopy = ( HB_USHORT ) hb_arrayLen( pFields );

         for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
         {
            const char * szFieldName = hb_arrayGetCPtr( pFields, uiIter );
            if( szFieldName )
            {
               int iPos = hb_rddFieldIndex( pArea, szFieldName );

               if( iPos )
               {
                  PHB_ITEM pFieldNum = hb_itemPutNI( NULL, iPos );
                  hb_itemArrayPut( pFields, uiIter, pFieldNum );
                  hb_itemRelease( pFieldNum );
                  continue;
               }
            }

            if( hb_arrayDel( pFields, uiIter ) )
            {
               hb_arraySize( pFields, hb_arrayLen( pFields ) - 1 );
               uiIter--;
               uiFieldCopy--;
            }

         }
      }


      if( ! bAsArray )
      {
         pFieldNames = hb_itemNew( NULL );
         if( bNoFieldPassed )
         {
            hb_arrayNew( pFieldNames, uiFields );
            for( uiIter = 1; uiIter <= uiFields; uiIter++ )
            {
               char * szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
               pField      = hb_itemNew( NULL );
               szName[ 0 ] = '\0';
               SELF_FIELDNAME( pArea, uiIter, szName );
               hb_itemPutCPtr( pField, szName );
               hb_arraySet( pFieldNames, uiIter, pField );
               hb_itemRelease( pField );
            }
         }
         else
         {
            hb_arrayNew( pFieldNames, uiFieldCopy );
            for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
            {
               char * szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
               pField      = hb_itemNew( NULL );
               szName[ 0 ] = '\0';
               SELF_FIELDNAME( pArea, ( HB_USHORT ) hb_itemGetNI( hb_arrayGetItemPtr( pFields, uiIter ) ), szName );
               hb_itemPutCPtr( pField, szName );
               hb_arraySet( pFieldNames, uiIter, pField );
               hb_itemRelease( pField );
            }
         }
      }

      uiRecCount = 0;
      while( ( nCount == 0 || uiRecCount <= nCount ) &&
             ( ! pWhile || hb_itemGetL( hb_vmEvalBlock( pWhile ) ) ) )
      {

         if( SELF_EOF( pArea, &bEof ) != HB_SUCCESS )
            break;

         if( bEof )
            break;

         if( ! pFor || hb_itemGetL( hb_vmEvalBlock( pFor ) ) )
         {

            if( bAsArray )
            {
               writeByte( context, ARRAY_TYPE );
               if( bNoFieldPassed )
               {
                  amf3_encode_int( context, ( ( int ) uiFields << 1 ) | REFERENCE_BIT );
                  writeByte( context, NULL_TYPE );
                  for( uiIter = 1; uiIter <= uiFields; uiIter++ )
                  {
                     SELF_GETVALUE( pArea, uiIter, pItem );
                     amf3_encode( context, pItem );
                  }
               }
               else
               {
                  amf3_encode_int( context, ( ( int ) uiFieldCopy << 1 ) | REFERENCE_BIT );
                  writeByte( context, NULL_TYPE );
                  for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
                  {
                     SELF_GETVALUE( pArea, ( HB_USHORT ) hb_itemGetNI( hb_arrayGetItemPtr( pFields, uiIter ) ) /* hb_arrayGetNI( pFields, uiIter ) */, pItem );
                     amf3_encode( context, pItem );
                  }
               }
            }
            else
            {

               PHB_ITEM pValue = hb_itemNew( NULL );

               writeByte( context, OBJECT_TYPE );
               /* amf3_encode_int(context, ((int) 1) << 1 | REFERENCE_BIT); */
               writeByte( context, DYNAMIC );
               writeByte( context, EMPTY_STRING_TYPE );
               if( bNoFieldPassed )
               {
                  for( uiIter = 1; uiIter <= uiFields; uiIter++ )
                  {
                     SELF_GETVALUE( pArea, uiIter, pValue );
                     amf3_serialize_string( context, hb_arrayGetItemPtr( pFieldNames, uiIter ) );
                     amf3_encode( context, pValue );
                  }
               }
               else
               {
                  for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
                  {
                     SELF_GETVALUE( pArea, ( HB_USHORT ) hb_itemGetNI( hb_arrayGetItemPtr( pFields, uiIter ) ), pValue );
                     amf3_serialize_string( context, hb_arrayGetItemPtr( pFieldNames, uiIter ) );
                     amf3_encode( context, pValue );
                  }
               }

               hb_itemRelease( pValue );
               writeByte( context, EMPTY_STRING_TYPE );
            }
            uiRecCount++;
         }

         if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
            break;

      }

      hb_itemRelease( pItem );

      if( ! bAsArray )
         hb_itemRelease( pFieldNames );

      context->cBuf = ( char * ) hb_xrealloc( context->cBuf, sizeof( char ) * context->position + 1 );

      hb_retclen_buffer( context->cBuf, context->position );

      context_release( context, outer_context );

      hb_xfree( context );

   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( AMF3_ENCODE )
{
   PHB_ITEM pItem    = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pFuncSym = hb_param( 2, HB_IT_SYMBOL );
   HB_BOOL  lBA      = hb_parldef( 3, HB_FALSE );
   HB_BOOL  lRetval;

   amfContext * context;

   if( ! pItem )
      return;

   context = ( amfContext * ) hb_xgrab( sizeof( amfContext ) );
   memset( context, 0, sizeof( amfContext ) );

   context->cBuf = ( char * ) hb_xgrab( sizeof( char ) * 8 );
   /* memset( context->cBuf, 0, sizeof( char ) * 8 ); */
   context->position      = 0;
   context->length        = sizeof( char ) * 8;
   context->str_rtrim     = HB_FALSE;
   context->obj_ref       = hb_hashNew( NULL );
   context->str_ref       = hb_hashNew( NULL );
   context->class_ref     = hb_hashNew( NULL );
   context->use_refs      = HB_TRUE;
   context->conv_function = pFuncSym;
   context->encode_ba     = lBA;
   hb_hashSetFlags( context->obj_ref, HB_HASH_KEEPORDER );
   hb_hashSetFlags( context->str_ref, HB_HASH_KEEPORDER );
   hb_hashSetFlags( context->class_ref, HB_HASH_KEEPORDER );

   /* "strstr" is another optional idea of catching similar strings,
      key in this hash is not the pointer to C char, but the string
      itself and the value is id of the reference */
   context->use_strstr   = HB_TRUE;
   context->strstr_count = 0;
   context->strstr_ref   = hb_hashNew( NULL );

   lRetval = amf3_encode( context, pItem );

   if( context->use_refs )
   {
      hb_itemRelease( context->obj_ref );
      hb_itemRelease( context->str_ref );
      hb_itemRelease( context->class_ref );
   }

   if( context->use_strstr )
      hb_itemRelease( context->strstr_ref );

   /*if(context->conv_function)
      hb_itemRelease(context->conv_function);*/

   if( ! lRetval )
   {
      hb_xfree( context->cBuf );
      hb_xfree( context );
      return;
   }

   context->cBuf = ( char * ) hb_xrealloc( context->cBuf, sizeof( char ) * context->position + 1 );

   hb_retclen_buffer( context->cBuf, context->position );
   hb_xfree( context );
}
