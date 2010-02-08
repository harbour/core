/*
 * $Id$
 */

#include "hbapigt.h"

void main( void )
{
   HB_BYTE * test = "Testing GT API Functions";
   HB_BYTE * test2 = "This message wraps!";
   int iRow, iCol;

   /* NOTE: always have to initialze video subsystem */
   hb_gtInit();

   /* save screen (doesn't work under DOS) */
   /*
   HB_BYTE * scr;
   HB_SIZE size;

   hb_gtRectSize( 1, 1, hb_gtMaxRow(), hb_gtMaxCol(), &size );
   scr = ( HB_BYTE * ) hb_xgrab( size );
   hb_gtSave( 1, 1, hb_gtMaxRow() - 1, hb_gtMaxCol() - 1, scr );
   */

   /* writing text */
   hb_gtSetPos( 3, 3 );
   hb_gtWrite( test, strlen( test ) );
   hb_gtSetPos( 12, 42 );
   hb_gtWrite( test, strlen( test ) );

   /* wrapping text */
   hb_gtSetPos( 7, 70 );
   hb_gtWrite( test2, strlen( test2 ) );

   /* writing color text */
   hb_gtSetColorStr( "W+/B, B/W" );
   hb_gtColorSelect( _CLR_STANDARD );
   hb_gtWrite( "Enhanced color (B/W)", 20 );
   hb_gtSetPos( 22, 62 );
   hb_gtColorSelect( _CLR_ENHANCED );
   hb_gtWrite( "Standard Color (W+/B)", 21 );

   /* boxes */
   hb_gtBoxS( 10, 10, 20, 20 );
   hb_gtBoxD( 10, 40, 15, 45 );

   /* cursor functions */
   hb_gtSetPos( 12, 1 );

   /* none */
   hb_gtSetCursor( _SC_NONE );
   getch();

   /* underline */
   hb_gtSetCursor( _SC_NORMAL );
   getch();

   /* lower half block */
   hb_gtSetCursor( _SC_INSERT );
   getch();

   /* full block */
   hb_gtSetCursor( _SC_SPECIAL1 );
   getch();

   /* upper half block */
   hb_gtSetCursor( _SC_SPECIAL2 );
   getch();

   /* restore screen (doesn't work under DOS) */
   /*
   hb_gtRest( 1, 1, hb_gtMaxRow() - 1, hb_gtMaxCol() - 1, scr );
   hb_xfree( scr );
   */
}
