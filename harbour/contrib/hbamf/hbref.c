/*******
 *
 *  hbref.c by Aleksander Czajczynski <hb/at/fki.pl> 2011-2012
 *
 *  hbref.c - Using Harbour C-pointers to various items as hashkeys
 *
 ********/

#include "hbapiitm.h"

void _ref_realItemPtr( PHB_ITEM pKey, PHB_ITEM pItem )
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
