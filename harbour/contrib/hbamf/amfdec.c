/*
 * $Id$
 */

/*******
 *
 *  by Aleksander Czajczynski <hb/at/fki.pl> 2011-2012
 *
 *  Decoding AMF3 to Harbour items
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
#include "amf.h"

#include "hbdate.h"
#include "hbmath.h"

#include "hbvm.h"

typedef struct
{
   const char * cBuf;
   HB_ISIZ      position;
   HB_ISIZ      length;
   PHB_ITEM     obj_ref;
   PHB_ITEM     str_ref;
   PHB_ITEM     class_ref;
   PHB_ITEM     conv_function;
} amfContext;

static HB_BOOL amf3_getItem( amfContext * context, PHB_ITEM pItem );
extern HB_BOOL hbamf_is_cls_externalizable( HB_USHORT uiClass );

static PHB_ITEM hbamf_cls_externalizable_instance( PHB_ITEM pClassFuncStr )
{
   PHB_DYNS pSymbol = hb_dynsymGet( hb_itemGetCPtr( pClassFuncStr ) );

   if( pSymbol )
   {
      PHB_ITEM pRetCopy = hb_itemNew( NULL );
      PHB_ITEM pNewItem = hb_itemNew( NULL );
      hb_itemMove( pRetCopy, hb_stackReturnItem() );

      hb_vmPushDynSym( pSymbol );
      hb_vmPushNil();
      hb_vmDo( 0 );

      hb_objSendMsg( hb_stackReturnItem(), "NEW", 0 );

      hb_itemMove( pNewItem, hb_stackReturnItem() );
      hb_itemMove( hb_stackReturnItem(), pRetCopy );

      hb_itemRelease( pRetCopy );

      if( pNewItem )
      {
         if( ! HB_IS_OBJECT( pNewItem ) )
         {
            hb_itemRelease( pNewItem );
            pNewItem = NULL;
         }
      }

      return pNewItem;
   }

   return NULL;
}

static char * readByte( amfContext * context )
{
   HB_ISIZ new_position = context->position + 1;
   char *  byte_ref;

   if( new_position < 0 || new_position > context->length )
      return NULL;

   byte_ref = ( char * ) ( context->cBuf + context->position );
   context->position = new_position;
   return byte_ref;
}

static char * readBytes( amfContext * context, HB_ISIZ len )
{
   HB_ISIZ new_position = context->position + len;
   char *  result;

   if( new_position < 0 || new_position > context->length )
      return NULL;

   result = ( char * ) ( context->cBuf + context->position );
   context->position = new_position;
   return result;
}

static HB_BOOL amfX_decode_double( amfContext * context, double * val )
{
   #ifndef HB_BIG_ENDIAN
   char c_val[ 8 ];
   #endif
   const char * bytes = readBytes( context, 8 );
   if( ! bytes )
      return HB_FALSE;

   /*
    * Put bytes from byte array into double
    * TOFIX: does this aligment work on any platform?

      union aligned {
       double d_val;
       char c_val[8];
      } d;

    */

   /* AMF transmitted double is always in network byte order */
#ifdef HB_BIG_ENDIAN
   memcpy( val, bytes, 8 );
#else
   /* Flip endianness */
   c_val[ 0 ] = bytes[ 7 ];
   c_val[ 1 ] = bytes[ 6 ];
   c_val[ 2 ] = bytes[ 5 ];
   c_val[ 3 ] = bytes[ 4 ];
   c_val[ 4 ] = bytes[ 3 ];
   c_val[ 5 ] = bytes[ 2 ];
   c_val[ 6 ] = bytes[ 1 ];
   c_val[ 7 ] = bytes[ 0 ];
   memcpy( val, c_val, 8 );
#endif

   return HB_TRUE;
}

static HB_BOOL amf3_decode_int( amfContext * context, int * iVal )
{
   int    result   = 0;
   int    byte_cnt = 0;
   char * byte_ref;
   char   byte;

   byte_ref = readByte( context );
   if( ! byte_ref )
      return HB_FALSE;

   byte = byte_ref[ 0 ];

   /* If 0x80 is set, int includes the next byte, up to 4 total bytes */
   while( ( byte & 0x80 ) && ( byte_cnt < 3 ) )
   {
      result <<= 7;
      result  |= byte & 0x7F;
      byte_ref = readByte( context );
      if( ! byte_ref )
         return HB_FALSE;
      byte = byte_ref[ 0 ];
      byte_cnt++;
   }

   /* shift bits in last byte */
   if( byte_cnt < 3 )
   {
      result <<= 7;   /* shift by 7, since the 1st bit is reserved for next byte flag */
      result  |= byte & 0x7F;
   }
   else
   {
      result <<= 8;   /* shift by 8, since no further bytes are possible and 1st bit is not used for flag. */
      result  |= byte & 0xff;
   }

   /* Move sign bit, since we're converting 29bit->32bit */
   if( result & 0x10000000 )
      result -= 0x20000000;

   *iVal = result;
   return HB_TRUE;
}

#if 0

static HB_BOOL amf3_decode_reference( PHB_ITEM pHash, int val, PHB_ITEM pRefItem )
{
   /* Check for index reference */
   if( ( val & REFERENCE_BIT ) == 0 )
   {
      PHB_ITEM pKey = hb_itemNew( NULL );
      hb_itemPutNI( pKey, val >> 1 );

      pRefItem = hb_hashGetItemPtr( pHash, pKey, 0 );

      hb_itemRelease( pKey );
      return HB_TRUE;
   }

   return HB_FALSE;
}

#endif

static PHB_ITEM amf3_decode_reference( PHB_ITEM pHash, int val )
{
   /* Check for index reference */
   if( ( val & REFERENCE_BIT ) == 0 )
   {
      PHB_ITEM pKey = hb_itemNew( NULL );
      PHB_ITEM pRefItem;
      hb_itemPutNI( pKey, val >> 1 );

      pRefItem = hb_hashGetItemPtr( pHash, pKey, 0 );
      if( ! pRefItem )
         hb_itemPutL( pRefItem, HB_FALSE );

      hb_itemRelease( pKey );
      return pRefItem;
   }

   return NULL;
}

static void amf3_add_reference( PHB_ITEM pHash, PHB_ITEM pItem )
{
   HB_SIZE  iRef = hb_hashLen( pHash );
   PHB_ITEM pKey = hb_itemNew( NULL );

   /* the reference id in AMF starts from 0, and increase
      sequentially - this means we can also use an array,
      not hash for the purpose */

   hb_itemPutNS( pKey, /* ++ first one was 0 */ iRef );
   hb_hashAdd( pHash, pKey, pItem );

   hb_itemRelease( pKey );
}

static HB_BOOL amfX_decode_string( amfContext * context, PHB_ITEM pItem, unsigned int string_size )
{
   const char * str = readBytes( context, string_size );

   if( ! str )
      return HB_FALSE;

   hb_itemPutStrLenUTF8( pItem, str, string_size );
   return HB_TRUE;
}

static HB_BOOL amf3_deserialize_string( amfContext * context, PHB_ITEM pItem )
{
   int      header;
   int *    header_p = &header;
   PHB_ITEM pRefItem;
   PHB_ITEM pHash = context->str_ref;

   if( ! amf3_decode_int( context, header_p ) )
      return HB_FALSE;

   /* Check for null string */
   if( header == EMPTY_STRING_TYPE )
   {
      hb_itemPutC( pItem, NULL );
      return HB_TRUE;
   }

   /* Check for reference */
   pRefItem = amf3_decode_reference( pHash, header );
   if( pRefItem )
   {
      if( HB_IS_LOGICAL( pRefItem ) )
      {
         /* Logical value means a problem getting reference from the hash */
         hb_itemRelease( pRefItem );
         return HB_FALSE;
      }
      /* Copies string from reference hash pRefItem -> pItem */
      hb_itemCopy( pItem, pRefItem );
      return HB_TRUE;
   }

   /* No reference found */
   if( ! amfX_decode_string( context, pItem, header >> 1 ) )
      return HB_FALSE;

   /* Adds reference */
   amf3_add_reference( pHash, pItem );

   return HB_TRUE;
}

/* Add the dynamic attributes of an encoded obj to a dict. */
static HB_BOOL amf3_decode_dynamic_dict( amfContext * context, PHB_ITEM pItem )
{
   PHB_ITEM pKey;
   PHB_ITEM pValue;
   HB_BOOL  result;

   for( ;; )
   {
      pKey = hb_itemNew( NULL );
      if( ! amf3_deserialize_string( context, pKey ) )
      {
         hb_itemRelease( pKey );
         return HB_FALSE;
      }

      if( hb_itemGetCLen( pKey ) == 0 )
      {
         /* Empty string marks end of name/value pairs */
         hb_itemRelease( pKey );
         return HB_TRUE;
      }

      pValue = hb_itemNew( NULL );
      if( ! amf3_getItem( context, pValue ) )
      {
         hb_itemRelease( pKey );
         hb_itemRelease( pValue );
         return HB_FALSE;
      }

      result = hb_hashAdd( pItem, pKey, pValue );
      hb_itemRelease( pKey );
      hb_itemRelease( pValue );
      if( ! result )
         return HB_FALSE;
   }
}

/* Populate an array with vals from the buffer. */
static HB_BOOL decode_dynamic_array_AMF3( amfContext * context, PHB_ITEM pItem, int array_len, HB_BOOL dict )
{
   int     i;
   HB_BOOL lRet;

   if( dict )
   {
      /* Object is a dict, set item index as key. */
      for( i = 0; i < array_len; i++ )
      {
         PHB_ITEM pValue = hb_itemNew( NULL );
         lRet = HB_FALSE;

         if( amf3_getItem( context, pValue ) )
         {
            PHB_ITEM pKey = hb_itemNew( NULL );
            hb_itemPutNI( pKey, i );

            if( hb_hashAdd( pItem, pKey, pValue ) )
               lRet = HB_TRUE;

            hb_itemRelease( pKey );
         }

         hb_itemRelease( pValue );

         if( ! lRet )
            return HB_FALSE;
      }
   }
   else
   {
      /* Standard array. */
      for( i = 0; i < array_len; i++ )
      {
         PHB_ITEM pValue = hb_itemNew( NULL );
         lRet = HB_FALSE;

         if( amf3_getItem( context, pValue ) )
         {
            if( hb_arraySet( pItem, i + 1, pValue ) )
               lRet = HB_TRUE;
         }

         hb_itemRelease( pValue );

         if( ! lRet )
            return HB_FALSE;
      }
   }

   return HB_TRUE;
}

static HB_BOOL amf3_deserialize_array( amfContext * context, PHB_ITEM pItem, HB_BOOL collection )
{
   int      header;
   int *    header_p = &header;
   PHB_ITEM pRefItem;
   PHB_ITEM pHash = context->obj_ref;
   int      array_len;
   HB_BOOL  mixed; /* if the result will be a Hash with both numbers and strings as keys */
   char *   byte_ref;

   if( ! amf3_decode_int( context, header_p ) )
      return HB_FALSE;

   /* Check for reference */
   pRefItem = amf3_decode_reference( pHash, header );
   if( pRefItem )
   {
      if( HB_IS_LOGICAL( pRefItem ) )
      {
         /* Logical value means a problem getting reference from the hash */
         hb_itemRelease( pRefItem );
         return HB_FALSE;
      }

      /* Reference found */
      if( collection )
      {
         /* Map ArrayCollection idx to ref, since
            it points to the same list. */
         amf3_add_reference( pHash, pRefItem );
      }
      /* Copies string from reference hash pRefItem -> pItem */
      hb_itemCopy( pItem, pRefItem );
      return HB_TRUE;
   }

   array_len = ( int ) ( header >> 1 );
   /* Original python comment was:
      Can't use array_len to create a list of known
      length, see ticket #46
      I think that this is not a problem for Harbour */

   /* Determine if array is mixed (associative) or not */
   mixed    = HB_FALSE;
   byte_ref = readByte( context );
   if( byte_ref == NULL )
      return HB_FALSE;

   if( byte_ref[ 0 ] == EMPTY_STRING_TYPE )
   {
      /* Dense array */
      hb_arrayNew( pItem, array_len );
   }
   else
   {
      context->position--;
      hb_hashNew( pItem );
      hb_hashSetFlags( pHash, HB_HASH_KEEPORDER );
      hb_hashPreallocate( pItem, array_len );

      if( ! amf3_decode_dynamic_dict( context, pItem ) )
         return HB_FALSE;

      mixed = HB_TRUE;
   }

   amf3_add_reference( pHash, pItem );

   /* Originally a python comment.
      I don't understand reasons for following,
      but let it be like this.
      ADD: oh, maybe it's because parsing of
      ArrayCollection starts as Object
      --
      If this is an ArrayCollection,
      we need to add another reference,
      so there is one that
      points to the array and one that points
      to the collection.
    */

   if( collection )
      amf3_add_reference( pHash, pItem );

   return decode_dynamic_array_AMF3( context, pItem, array_len, mixed );

   /* return HB_TRUE; */
}

/* Decode a date. */
static HB_BOOL amf3_decode_epoch( amfContext * context, PHB_ITEM pItem )
{
   double   dJulian, dTime;
   double   epoch_millisecs;
   double * epoch_p = &epoch_millisecs;

   if( ! amfX_decode_double( context, epoch_p ) )
      return HB_FALSE;

   /* 210866760000000 unix_epoch milliseconds base in JD */
   dTime = modf( ( 210866760000000 + epoch_millisecs + 0.5 ) / HB_MILLISECS_PER_DAY, &dJulian );

   hb_itemPutTDT( pItem, ( long ) dJulian, ( long ) ( dTime * HB_MILLISECS_PER_DAY ) );

   /* TOFIX: why following Harbour function doesn't work out of the box?
      C compiler was MSVC 7.1 (Visual Studio 2003)
      hb_itemPutTD( pItem, epoch_millisecs + 210866803200000 );
    */

   return HB_TRUE;
}

/* Deserialize date. */
static HB_BOOL amf3_deserialize_date( amfContext * context, PHB_ITEM pItem )
{
   int      header;
   int *    header_p = &header;
   PHB_ITEM pRefItem;
   PHB_ITEM pHash = context->obj_ref;

   if( ! amf3_decode_int( context, header_p ) )
      return HB_FALSE;

   /* Check for reference */
   pRefItem = amf3_decode_reference( pHash, header );
   if( pRefItem )
   {
      if( HB_IS_LOGICAL( pRefItem ) )
      {
         /* Logical value means a problem getting reference from the hash */
         hb_itemRelease( pRefItem );
         return HB_FALSE;
      }
      /* Copies date from reference hash pRefItem -> pItem */
      hb_itemCopy( pItem, pRefItem );
      return HB_TRUE;
   }

   if( ! amf3_decode_epoch( context, pItem ) )
      return HB_FALSE;

   /* Add reference */
   amf3_add_reference( pHash, pItem );

   return HB_TRUE;
}

/* Decode a byte array. */
static HB_BOOL amf3_decode_byte_array( amfContext * context, PHB_ITEM pItem, int byte_len )
{
   const char * str = readBytes( context, byte_len );

   if( ! str )
      return HB_FALSE;

   hb_itemPutStrLen( pItem, hb_vmCDP(), str, byte_len );
   return HB_TRUE;
}

/* Deserialize a byte array. */
static HB_BOOL amf3_deserialize_byte_array( amfContext * context, PHB_ITEM pItem )
{
   int      header;
   int *    header_p = &header;
   PHB_ITEM pRefItem;
   PHB_ITEM pHash = context->obj_ref;

   if( ! amf3_decode_int( context, header_p ) )
      return HB_FALSE;

   /* Check for reference */
   pRefItem = amf3_decode_reference( pHash, header );
   if( pRefItem )
   {
      if( HB_IS_LOGICAL( pRefItem ) )
      {
         /* Logical value means a problem getting reference from the hash */
         hb_itemRelease( pRefItem );
         return HB_FALSE;
      }
      /* Copies ByteArray (string) from reference hash pRefItem -> pItem */
      hb_itemCopy( pItem, pRefItem );
      return HB_TRUE;
   }

   if( ! amf3_decode_byte_array( context, pItem, header >> 1 ) )
      return HB_FALSE;

   /* Add reference */
   amf3_add_reference( pHash, pItem );

   return HB_TRUE;
}

/* Get an object's class def - nearly a copy/paste from decoder */
static PHB_ITEM class_def_from_classname( /* amfContext * context, */ PHB_ITEM pClassName )
{
   HB_USHORT uiClass;
   PHB_ITEM  pClass;
   PHB_ITEM  pKey;
   PHB_ITEM  pValue;
   char *    pszBuffer = hb_itemGetC( pClassName );
   HB_SIZE   nLen      = hb_itemGetCLen( pClassName );

   hb_strUpper( pszBuffer, nLen );

   /* get Harbour's class id/handle */
   uiClass = hb_clsFindClass( pszBuffer, NULL );

   hb_strfree( pszBuffer );

   /* uiClass = hb_objGetClass( pItem ); */
   if( ! uiClass )
      return NULL;

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

   return pClass;
}

/*
 * Decode a ClassDef.
 *
 * Header argument is the obj header.
 */
static HB_BOOL amf3_decode_class_def( amfContext * context, PHB_ITEM pClass, int header )
{
   PHB_ITEM pStrAlias       = hb_itemNew( NULL );
   PHB_ITEM pMappedClassDef = NULL;
   PHB_ITEM pKey;
   PHB_ITEM pValue;
   PHB_ITEM pAttrs;
   int      static_attr_len;
   int      i;

   if( ! amf3_deserialize_string( context, pStrAlias ) )
   {
      hb_itemRelease( pStrAlias );
      return HB_FALSE;
   }

   /* If this alias string is empty then the object is anonymous */
   if( hb_itemGetCLen( pStrAlias ) > 0 )
   {
      /* Retrieve a ClassDef from a class alias string. */
      pMappedClassDef = class_def_from_classname( pStrAlias );
      if( ! pMappedClassDef )
      {
         pMappedClassDef = hb_itemNew( NULL );
         hb_hashNew( pMappedClassDef ); /* empty hash emulation for now */
      }

      /* PyObject_CallMethodObjArgs(context->class_mapper,
          context->class_def_name, alias, NULL); */
   }
   hb_itemRelease( pStrAlias );

   /* Create a dict with class def information
      specific to this decode context. */
   hb_hashNew( pClass );

   if( pMappedClassDef )
   {
      pKey = hb_itemPutC( NULL, "class_def" ); /* hb_itemNew( NULL ); */

      if( ! hb_hashAdd( pClass, pKey, pMappedClassDef ) )
      {
         hb_itemRelease( pKey );
         hb_itemRelease( pMappedClassDef );
         return HB_FALSE;
      }
      hb_itemRelease( pKey );

      pKey = hb_itemPutC( NULL, "EXTERNALIZABLE_CLASS_DEF" );
      if( hb_hashScan( pMappedClassDef, pKey, NULL ) )
      {
         /* There is nothing else we need to do
            with externalizable ClassDefs */
         hb_itemRelease( pKey );
         hb_itemRelease( pMappedClassDef );
         return HB_TRUE;
      }
      hb_itemRelease( pKey );

      /* this item should be now referenced in the hash */
      hb_itemRelease( pMappedClassDef );
   }

   if( ( header & 0x07FFFFFF ) == EXTERNALIZABLE )
   {
      /* If the class is externalizable, but the ClassDef isn't,
         we have a big problem, because we don't know how to read
         the raw bytes. */

      /* TODO: introduce similar RTE?
         PyErr_SetString(amfast_DecodeError, "Encoded class is externalizable, but ClassDef is not."); */
      return HB_FALSE;
   }


   /* Set dynamic flag */
   pKey   = hb_itemPutC( NULL, "dynamic" );
   pValue = hb_itemPutL( NULL, ( header & DYNAMIC ) == DYNAMIC );
   if( ! hb_hashAdd( pClass, pKey, pValue ) )
   {
      hb_itemRelease( pKey );
      hb_itemRelease( pValue );
      return HB_FALSE;
   }
   hb_itemRelease( pKey );
   hb_itemRelease( pValue );

   /* Decode static attr names */
   static_attr_len = ( int ) ( header >> 4 );
   pAttrs = hb_itemNew( NULL );
   hb_arrayNew( pAttrs, static_attr_len );

   for( i = 0; i < static_attr_len; i++ )
   {
      pValue = hb_itemNew( NULL );
      if( ! amf3_deserialize_string( context, pValue ) )
      {
         hb_itemRelease( pAttrs );
         hb_itemRelease( pValue );
         return HB_FALSE;
      }

      /* steals ref to attr_name */
      if( ! hb_arraySet( pAttrs, i + 1, pValue ) )
      {
         hb_itemRelease( pAttrs );
         hb_itemRelease( pValue );
         return HB_FALSE;
      }

      hb_itemRelease( pValue );
   }

   /* Set decoded attrs onto ClassDef */
   pKey = hb_itemPutC( NULL, "static_attrs" );
   if( ! hb_hashAdd( pClass, pKey, pAttrs ) )
   {
      hb_itemRelease( pKey );
      hb_itemRelease( pAttrs );
      return HB_FALSE;
   }
   hb_itemRelease( pKey );
   hb_itemRelease( pAttrs );

   return HB_TRUE;
}

/*
 * Deserialize a ClassDef.
 *
 * header argument is the parsed obj header.
 */
static HB_BOOL amf3_deserialize_class_def( amfContext * context, PHB_ITEM pClass, int header )
{
   PHB_ITEM pHash = context->class_ref;
   PHB_ITEM pRefItem;

   /* Check for reference */
   pRefItem = amf3_decode_reference( pHash, header );
   if( pRefItem )
   {
      if( HB_IS_LOGICAL( pRefItem ) )
      {
         /* Logical value means a problem getting reference from the hash */
         hb_itemRelease( pRefItem );
         return HB_FALSE;
      }
      /* Copies ClassDef from reference hash pRefItem -> pClass */
      hb_itemCopy( pClass, pRefItem );
      return HB_TRUE;
   }

   if( ! amf3_decode_class_def( context, pClass, header ) )
      return HB_FALSE;

   /* Add reference to obj */
   amf3_add_reference( pHash, pClass );

   return HB_TRUE;
}

/* Returns a dict with vals from an obj. */
static HB_BOOL amf3_decode_obj_attrs( amfContext * context, PHB_ITEM pHash, PHB_ITEM pClass )
{
   PHB_ITEM pArray;
   PHB_ITEM pKey;
   PHB_ITEM pValue;
   HB_BOOL  result;
   HB_SIZE  static_attr_len;
   HB_SIZE  i;

   /* Put decoded attributes into pHash */

   /* Decode static attrs */
   pArray = hb_hashGetCItemPtr( pClass, "static_attrs" );
   if( ! pArray )
      return HB_FALSE;

   /* maybe hb_arrayGetItemPtr(?) could be used */

   static_attr_len = hb_arrayLen( pArray );

   for( i = 0; i < static_attr_len; i++ )
   {
      pValue = hb_itemNew( NULL );
      if( ! amf3_getItem( context, pValue ) )
      {
         hb_itemRelease( pValue );
         return HB_FALSE;
      }

      pKey = hb_itemNew( NULL );
      if( ! hb_arrayGet( pArray, i + 1, pKey ) )
      {
         hb_itemRelease( pValue );
         hb_itemRelease( pKey );
         return HB_FALSE;
      }

      result = hb_hashAdd( pHash, pKey, pValue );
      hb_itemRelease( pValue );
      hb_itemRelease( pKey );

      if( ! result )
         return HB_FALSE;
   }

   /* Decode dynamic attrs */
   pValue = hb_hashGetCItemPtr( pClass, "dynamic" );
   if( ! pValue )
      return HB_FALSE;

   if( HB_IS_LOGICAL( pValue ) && hb_itemGetL( pValue ) )
   {
      if( ! amf3_decode_dynamic_dict( context, pHash ) )
         return HB_FALSE;
   }

   return HB_TRUE;
}

/* Decode an anonymous obj. */
static HB_BOOL amf3_decode_anon_obj( amfContext * context, PHB_ITEM pItem, PHB_ITEM pClass )
{
   PHB_ITEM pAnonHash = hb_itemNew( NULL );
   HB_BOOL  result    = HB_FALSE;

   /* Original python comment which I don't understand:
      We're using merge instead of populating the dict
      directly, because we have to setup a reference to the
      object before decoding it. ?????? */

   /* we (Harbourers) are supplying already initialized hash to next func */
   if( hb_arrayGet( pItem, OBJAMF_VAR_HASH, pAnonHash ) )
      result = amf3_decode_obj_attrs( context, pAnonHash, pClass );

   hb_itemRelease( pAnonHash );

   return result;
}

static HB_BOOL amf3_decode_externalizable( amfContext * context, PHB_ITEM pItem )
{
   const char * position;
   PHB_ITEM     pRetCopy = hb_itemNew( NULL );
   PHB_ITEM     pStr, pPos;
   HB_BOOL      result = HB_TRUE;
   PHB_ITEM     pObject;

   if( pItem == hb_stackReturnItem() )
      pObject = pRetCopy;
   else
      pObject = pItem;

   hb_itemMove( pRetCopy, hb_stackReturnItem() );

   position = context->cBuf + context->position;
   pStr     = hb_itemNew( NULL );
   pStr     = hb_itemPutCLConst( pStr, position, context->length - context->position );
   pPos     = hb_objSendMsg( pObject, "READEXTERNAL", 1, pStr );
   if( HB_IS_INTEGER( pPos ) )
   {
      if( ! readBytes( context, hb_itemGetNI( pPos ) ) )
      {
         result = HB_FALSE;
      }
   }
   else
   {
      result = HB_FALSE;
   }

   hb_itemMove( hb_stackReturnItem(), pRetCopy );

   hb_itemRelease( pRetCopy );
   hb_itemRelease( pStr );
   return result;
}

/*
 * Deserialize an obj.
 *
 * proxy is flag indicating that the obj being deserialized is within an ObjectProxy
 */
static HB_BOOL amf3_deserialize_obj( amfContext * context, PHB_ITEM pItem, HB_BOOL proxy )
{
   int      header;
   int *    header_p = &header;
   PHB_ITEM pRefItem;
   PHB_ITEM pHash = context->obj_ref;
   PHB_ITEM pClass;
   PHB_ITEM pMappedClassDef;
   PHB_ITEM pValue;
   int      obj_type; /* 0 = anonymous, 1 == externalizable, 2 == typed */
   HB_BOOL  result;

   if( ! amf3_decode_int( context, header_p ) )
      return HB_FALSE;

   /* Check for reference */
   pRefItem = amf3_decode_reference( pHash, header );
   if( pRefItem )
   {
      if( HB_IS_LOGICAL( pRefItem ) )
      {
         /* Logical value means a problem getting reference from the hash */
         hb_itemRelease( pRefItem );
         return HB_FALSE;
      }

      /* Reference found */
      if( proxy )
      {
         /* Map ObjectProxy idx to ref, since
            it points to the same obj. */
         amf3_add_reference( pHash, pRefItem );
      }

      /* Copies ByteArray (string) from reference hash pRefItem -> pItem */
      hb_itemCopy( pItem, pRefItem );
      return HB_TRUE;
   }

   pClass = hb_itemNew( NULL );
   if( ! amf3_deserialize_class_def( context, pClass, header ) )
   {
      hb_itemRelease( pClass );
      return HB_FALSE;
   }

   pMappedClassDef = hb_hashGetCItemPtr( pClass, "class_def" );
   if( ! pMappedClassDef )
   {
      /* Anonymous obj. */
      obj_type = 0;
   }
   else if( hb_hashGetCItemPos( pMappedClassDef, "EXTERNALIZABLE_CLASS_DEF" ) != 0 )
   {
      if( hb_hashGetCItemPos( pMappedClassDef, "ARRAY_COLLECTION_CLASS_DEF" ) != 0 )
      {
         hb_itemRelease( pClass );

         if( ! readByte( context ) ) /* Skip array type marker */
         {
            return HB_FALSE;
         }

         return amf3_deserialize_array( context, pItem, HB_TRUE );
      }

      if( hb_hashGetCItemPos( pMappedClassDef, "OBJECT_PROXY_CLASS_DEF" ) != 0 )
      {
         hb_itemRelease( pClass );

         if( ! readByte( context ) ) /* Skip array type marker */
         {
            return HB_FALSE;
         }

         return amf3_deserialize_obj( context, pItem, HB_TRUE );
      }

      obj_type = 1;
   }
   else
      obj_type = 2;

   /* Instantiate new obj */
   if( obj_type == 0 )
   {
      /* Anonymous obj == OBJAMF */
      hb_arrayNew( pItem, OBJAMF_VAR_COUNT );
      /* performance TOFIX, cache class id (in context maybe)
         to not scan all classes by name everytime */
      hb_objSetClass( pItem, "AMF_OBJ", "AMF_OBJ" );
      pValue = hb_itemPutNI( NULL, OBJAMF_VER );
      hb_arraySet( pItem, OBJAMF_VAR_VER, pValue );
      hb_itemRelease( pValue );
      pValue = hb_itemPutC( NULL, "ANONYMOUS" );
      hb_arraySet( pItem, OBJAMF_VAR_NAME, pValue );
      hb_itemRelease( pValue );
      pValue = hb_hashNew( NULL );
      hb_arraySet( pItem, OBJAMF_VAR_HASH, pValue );
      hb_itemRelease( pValue );
   }
   else if( obj_type == 1 )
   {
      /* externalizable object should allow a constructor without parameters,
         and the object should not do anything dangerous, because it could
         be instantiated on clients request */

      pValue = hb_hashGetCItemPtr( pMappedClassDef, "alias" );

      if( ! pValue )
      {
         hb_itemRelease( pClass );
         return HB_FALSE;
      }

      pValue = hbamf_cls_externalizable_instance( pValue );
      if( ! pValue )
      {
         hb_itemRelease( pClass );
         return HB_FALSE;
      }

      hb_itemMove( pItem, pValue );
      hb_itemRelease( pValue );
   }
   else
   {
      /* Create obj_val for all typed objs. */
      /* obj_val = PyObject_CallMethod(class_def, "getInstance", NULL); */
   }

   if( ! HB_IS_OBJECT( pItem ) )
   {
      hb_itemRelease( pClass );
      return HB_FALSE;
   }

   /* Reference must be added before children (to allow for recursion). */
   amf3_add_reference( pHash, pItem );

   if( proxy )
   {
      /*  If this is an ObjectProxy,
          we need to add another reference,
          so there is one that
          points to the obj and one that points
          to the proxy. */

      amf3_add_reference( pHash, pItem );
   }

   result = HB_FALSE;
   if( obj_type == 0 )
   {
      result = amf3_decode_anon_obj( context, pItem, pClass );
   }
   else if( obj_type == 1 )
   {
      /* result = HB_TRUE; */
      result = amf3_decode_externalizable( context, pItem /*, pMappedClassDef */ );
   }
   else if( obj_type == 2 )
   {
      /* result = decode_typed_obj_AMF3(context, obj_val, class_def_dict); */
   }

   hb_itemRelease( pClass );

   return result;
}

static void amf3_conversion_in( amfContext * context, PHB_ITEM pItem )
{
   PHB_ITEM pRetCopy = hb_itemNew( NULL );
   PHB_SYMB pSym     = hb_itemGetSymbol( context->conv_function );

   if( pItem == hb_stackReturnItem() )
   {
      hb_vmPushSymbol( pSym );
      hb_vmPushNil();
      hb_vmPush( pItem );
      hb_vmDo( 1 );
   }
   else
   {
      hb_itemMove( pRetCopy, hb_stackReturnItem() );
      hb_vmPushSymbol( pSym );
      hb_vmPushNil();
      hb_vmPush( pItem );
      hb_vmDo( 1 );
      hb_itemMove( pItem, hb_stackReturnItem() );
      hb_itemMove( hb_stackReturnItem(), pRetCopy );
   }
   hb_itemRelease( pRetCopy );
}

/* much of deserialize_* functions are so much similar that we may
   generalize them in one, f.e. adding another parameter specifying
   pointer of a final decoding function... in case reference checking
   returns nothing */

static HB_BOOL amf3_getItem( amfContext * context, PHB_ITEM pItem )
{
   char byte;
   const char * byte_ref;
   HB_BOOL      lRet = HB_TRUE;

   byte_ref = readByte( context );

   if( ! byte_ref )
      return HB_FALSE;
   byte = byte_ref[ 0 ];

   switch( byte )
   {
      case UNDEFINED_TYPE:
      case NULL_TYPE:
         lRet = HB_TRUE;
         break;

      case FALSE_TYPE:
         hb_itemPutL( pItem, HB_FALSE );
         lRet = HB_TRUE;
         break;

      case TRUE_TYPE:
         hb_itemPutL( pItem, HB_TRUE );
         lRet = HB_TRUE;
         break;

      case INT_TYPE:
      {
         int iVal;
         if( amf3_decode_int( context, &iVal ) )
            hb_itemPutNI( pItem, iVal );
         else
            lRet = HB_FALSE;
      }
      break;

      case DOUBLE_TYPE:
      {
         double dVal;
         if( amfX_decode_double( context, &dVal ) )
            hb_itemPutND( pItem, dVal );
         else
            lRet = HB_FALSE;
      }
      break;

      case STRING_TYPE:
         lRet = amf3_deserialize_string( context, pItem );
         break;

      case XML_DOC_TYPE:
         /* don't touch xml encoding right now */
         lRet = amf3_deserialize_byte_array( context, pItem );
         break;

      case DATE_TYPE:
         lRet = amf3_deserialize_date( context, pItem );
         break;

      case ARRAY_TYPE:
         lRet = amf3_deserialize_array( context, pItem, HB_FALSE );
         break;

      case OBJECT_TYPE:
         lRet = amf3_deserialize_obj( context, pItem, HB_FALSE );
         break;

      case XML_TYPE:
         /* don't touch xml encoding right now */
         lRet = amf3_deserialize_byte_array( context, pItem );
         break;

      case BYTE_ARRAY_TYPE:
         lRet = amf3_deserialize_byte_array( context, pItem );
         break;

      case AMF3_AMF0:
         lRet = amf3_getItem( context, pItem );
         break;

      default:
         lRet = HB_FALSE;
         break;
   }

   if( context->conv_function )
      amf3_conversion_in( context, pItem );

   return lRet;

}

HB_FUNC( AMF3_DECODE )
{
   PHB_ITEM pItem = hb_stackReturnItem();

#if defined( _DEBUG )
   PHB_ITEM pDebugBlock = hb_param( 2, HB_IT_BLOCK );
#endif
   PHB_ITEM pFuncSym = hb_param( 2, HB_IT_SYMBOL );

   amfContext * context;

   const char * szBuffer = hb_parc( 1 );

   if( ! szBuffer )
      return;

   context = ( amfContext * ) hb_xgrab( sizeof( amfContext ) );
   memset( context, 0, sizeof( amfContext ) );

   context->cBuf          = szBuffer;
   context->position      = 0;
   context->length        = hb_parclen( 1 );
   context->obj_ref       = hb_hashNew( NULL );
   context->str_ref       = hb_hashNew( NULL );
   context->class_ref     = hb_hashNew( NULL );
   context->conv_function = pFuncSym;

   amf3_getItem( context, pItem );

#if defined( _DEBUG )
   if( pDebugBlock )
   {
      hb_vmPushEvalSym();
      hb_vmPush( pDebugBlock );
      hb_vmPush( context->obj_ref );
      hb_vmPush( context->str_ref );
      hb_vmPush( context->class_ref );
      hb_vmSend( ( HB_USHORT ) 3 );
   }
#endif

   hb_itemRelease( context->obj_ref );
   hb_itemRelease( context->str_ref );
   hb_itemRelease( context->class_ref );

   /*if(context->conv_function)
      hb_itemRelease(context->conv_function);*/

   hb_xfree( context );
}
