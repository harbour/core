
#include "hbapigt.h"

int main( void )
{
   const char * test = "Testing GT API Functions";
   const char * test2 = "This message wraps!";

   void * scr;
   HB_SIZE size;

   /* NOTE: always have to initialze video subsystem */
   hb_gtInit( 0, 0, 0 );

   /* save screen */

   hb_gtRectSize( 1, 1, hb_gtMaxRow(), hb_gtMaxCol(), &size );
   scr = hb_xgrab( size );
   hb_gtSave( 1, 1, hb_gtMaxRow() - 1, hb_gtMaxCol() - 1, scr );

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
   hb_gtColorSelect( HB_CLR_STANDARD );
   hb_gtWrite( "Enhanced color (B/W)", 20 );
   hb_gtSetPos( 22, 62 );
   hb_gtColorSelect( HB_CLR_ENHANCED );
   hb_gtWrite( "Standard Color (W+/B)", 21 );

   /* boxes */
   hb_gtBoxS( 10, 10, 20, 20 );
   hb_gtBoxD( 10, 40, 15, 45 );

   /* cursor functions */
   hb_gtSetPos( 12, 1 );

   /* none */
   hb_gtSetCursor( SC_NONE );
   hb_inkey( HB_TRUE, 0.0, INKEY_ALL );

   /* underline */
   hb_gtSetCursor( SC_NORMAL );
   hb_inkey( HB_TRUE, 0.0, INKEY_ALL );

   /* lower half block */
   hb_gtSetCursor( SC_INSERT );
   hb_inkey( HB_TRUE, 0.0, INKEY_ALL );

   /* full block */
   hb_gtSetCursor( SC_SPECIAL1 );
   hb_inkey( HB_TRUE, 0.0, INKEY_ALL );

   /* upper half block */
   hb_gtSetCursor( SC_SPECIAL2 );
   hb_inkey( HB_TRUE, 0.0, INKEY_ALL );

   /* restore screen */
   hb_gtRest( 1, 1, hb_gtMaxRow() - 1, hb_gtMaxCol() - 1, scr );
   hb_xfree( scr );

   return 0;
}
