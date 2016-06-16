/* This is an original work by Ted Means and is placed in the public domain.

      Rev 1.1   01 May 1995 03:05:00   TED
   Added typecast to tame compiler warning

      Rev 1.0   01 Feb 1995 03:02:00   TED
   Initial release
 */

#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( FT_DESCEND )
{
   PHB_ITEM iP     = hb_itemParam( 1 );
   HB_TYPE  uiType = hb_itemType( iP );

   PHB_ITEM iR = NULL;

   if( ( uiType & HB_IT_NUMERIC ) && ( uiType & HB_IT_DOUBLE ) )
      iR = hb_itemPutND( NULL, 0 - hb_itemGetND( iP ) );

   else if( uiType & HB_IT_NUMERIC )
      iR = hb_itemPutNL( NULL, 0 - hb_itemGetNL( iP ) );

   else if( uiType & HB_IT_DATE )
      iR = hb_itemPutNL( NULL, 0x4FD4C0L - hb_itemGetNL( iP ) );

   else if( uiType & HB_IT_TIMESTAMP )
      iR = hb_itemPutND( NULL, 0x4FD4C0L - hb_itemGetTD( iP ) );

   else if( uiType & HB_IT_LOGICAL )
      iR = hb_itemPutL( NULL, hb_itemGetL( iP ) ? HB_FALSE : HB_TRUE );

   else if( uiType & HB_IT_STRING )
   {
      HB_SIZE uiLen = hb_itemSize( iP );
      HB_SIZE n;

      char * pDescend = ( char * ) hb_xgrab( uiLen );

      hb_itemCopyC( iP, pDescend, uiLen );

      for( n = 0; n < uiLen; ++n )
         pDescend[ n ] = ( char ) 0 - pDescend[ n ];

      iR = hb_itemPutCL( NULL, pDescend, uiLen );

      hb_xfree( pDescend );
   }

   hb_itemRelease( iP );
   hb_itemReturnRelease( iR );
}
