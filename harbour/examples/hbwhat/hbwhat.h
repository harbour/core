/*
 * $Id$
 */

/* TOFIX: Hack to make it compile when UNICODE is set.
          This library doesn't support UNICODE yet (?), so we 
          disable it to avoid build problems. [vszakats] */
#ifdef UNICODE
   #undef UNICODE
#endif

//#define HB_PARWH( x )            hb_parnl( x )
//#define HB_RETWH( x )            hb_retnl( ( LONG ) x )
//#define HB_STORWH( x, y )        hb_stornl( ( LONG ) x, y )
//#define HB_ARRAYSETWH( x, y, z ) hb_arraySetNL( x, y, ( LONG ) z )
//#define HB_PARWI( x )            hb_parni( x )
//#define HB_RETWI( x )            hb_retni( x )
//#define HB_STORWI( x, y )        hb_storni( ( HB_PTRDIFF ) x, y )

#define HB_PARWH( x )            ( ISPOINTER( x ) ? hb_parptr( x ) : ( void * ) ( HB_PTRDIFF ) hb_parnint( x ) )
#define HB_RETWH( x )            hb_retptr( x )
#define HB_STORWH( x, y )        hb_storptr( x, y )
#define HB_ARRAYSETWH( x, y, z ) hb_arraySetPtr( x, y, z )
#define HB_PARWI( x )            ( ( HB_PTRDIFF ) hb_parnint( x ) )
#define HB_RETWI( x )            hb_retnint( ( HB_PTRDIFF ) x )
#define HB_STORWI( x, y )        hb_stornint( ( HB_PTRDIFF ) x, y )
