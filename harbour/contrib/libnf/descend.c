/*
 * $Id$
 */

/*
 * File......: DESCEND.C
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   01 May 1995 03:05:00   TED
 * Added typecast to tame compiler warning
 *
 *    Rev 1.0   01 Feb 1995 03:02:00   TED
 * Initial release
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_Descend()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Create a descending index key value
 *  $SYNTAX$
 *     FT_Descend( <exp> ) -> <value>
 *  $ARGUMENTS$
 *     <exp> is any expression of character, numeric, date, or logical type.
 *  $RETURNS$
 *     The inverse of <exp>
 *  $DESCRIPTION$
 *     This function is a replacement for CA-Clipper's Descend() function,
 *     which is known to produce memory corruption occassionally.
 *  $EXAMPLES$
 *     ? FT_Descend( 1 )      // Returns -1
 *  $SEEALSO$
 *     FT_XTOY()
 *  $END$
 */

#include <hbapi.H>
#include <hbapiitm.h>
//#include <FM.API>

HB_FUNC( FT_DESCEND)
{
#if defined(HB_OS_DOS) || defined(HB_OS_WIN_32)
   {

      auto PHB_ITEM iP       = hb_itemParam( 1 );
      auto USHORT uiType = hb_itemType( iP );

      auto PHB_ITEM iR;
      auto USHORT uiLen, n;
      auto char * pDescend;
   
      if ( ( uiType & HB_IT_NUMERIC ) && ( uiType & HB_IT_DOUBLE ) )
         iR = hb_itemPutND( 0, 0 - hb_itemGetND( iP ) );

      else if ( uiType & HB_IT_NUMERIC )
         iR = hb_itemPutNL( 0, 0 - hb_itemGetNL( iP ) );

      else if ( uiType & HB_IT_DATE )
         iR = hb_itemPutNL( 0, 0x4FD4C0L - hb_itemGetNL( iP ) );

      else if ( uiType & HB_IT_LOGICAL )
         iR = hb_itemPutL( 0, ( hb_itemGetL( iP ) > 0 ) ? 0 : 1 );

      else if ( uiType & HB_IT_STRING )
      {
         uiLen = hb_itemSize( iP );

         pDescend = ( char *) hb_xgrab( uiLen );

         hb_itemCopyC( iP, pDescend, uiLen );

         for ( n = 0; n < uiLen; n++ )
            pDescend[ n ] = ( char ) 0 - pDescend[ n ];

         iR = hb_itemPutCL( 0, pDescend, uiLen );

         hb_xfree( pDescend );
   }

      hb_itemReturn( iR );

      hb_itemRelease( iP );
      hb_itemRelease( iR );

   return;
   }
#endif
}
