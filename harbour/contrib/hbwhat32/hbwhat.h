/*
 * $Id$
 */

//#define HB_PARWH( x )  hb_parnl( x )
//#define HB_RETWH( x )  hb_retnl( ( LONG ) x )
//#define HB_STORWH( x, y ) hb_stornl( ( LONG ) x, y )

#define HB_PARWH( x )  hb_parptr( x )
#define HB_RETWH( x )  hb_retptr( x )
#define HB_STORWH( x, y ) hb_storptr( x, y )
