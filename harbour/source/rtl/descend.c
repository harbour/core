/*
 * $Id$
 */

#include <extend.h>
#include <itemapi.h>
#include <ctype.h>
#include <init.h>

extern STACK stack;

HARBOUR HB_DESCEND(void);

HB_INIT_SYMBOLS_BEGIN( Descend__InitSymbols )
{ "DESCEND", FS_PUBLIC, HB_DESCEND, 0 }
HB_INIT_SYMBOLS_END( Descend__InitSymbols );
#pragma Descend__InitSymbols
                            
char *hb_strdescend( char *string )
{
   char *s;

   if( string )
   {
      for( s = string; *s; ++s )
         *s = 256 - *s;
   }
   return string;
}

HARBOUR HB_DESCEND( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pItem = hb_param(1, IT_ANY );

      if( pItem )
      {
         if( IS_STRING( pItem ) )
            hb_retc( hb_strdescend( pItem->item.asString.value ) );
         else if( IS_DATE( pItem ) )
            hb_retnl( 5231808 - pItem->item.asDate.value );
         else if( IS_INTEGER( pItem ) )
            hb_retni( -1 * pItem->item.asInteger.value );
         else if( IS_LONG( pItem ) )
            hb_retnl( -1 * pItem->item.asLong.value );
         else if( IS_DOUBLE( pItem ) )
         {
            PHB_ITEM pReturn;

            pReturn = hb_itemPutND( NULL, -1 * pItem->item.asDouble.value );
            hb_itemReturn( pReturn );
            hb_itemRelease( pReturn );

/* It is dengerous to operate on the stack directly
            stack.Return.wDec = pItem->wDec;
*/
         }
         else if( IS_LOGICAL( pItem ) )
            hb_retl( !pItem->item.asLogical.value );
         else
            hb_retc( "NIL" );
      }
   }
}
