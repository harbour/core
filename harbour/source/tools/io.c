/*
 * $Id$
 */

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( CD )
{
   hb_retni( ISCHAR( 1 ) ? hb_fsChDir( ( BYTE * ) hb_parc( 1 ) ) : 0 );
}

HB_FUNC( MD )
{
   hb_retni( ISCHAR( 1 ) ? hb_fsMkDir( ( BYTE * ) hb_parc( 1 ) ) : 0 );
}

HB_FUNC( RD )
{
   hb_retni( ISCHAR( 1 ) ? hb_fsRmDir( ( BYTE * ) hb_parc( 1 ) ) : 0 );
}

HB_FUNC( DISKUSED )
{
   hb_retnlen( hb_fsDiskSpace( ISNUM( 1 ) ? hb_parni( 1 ) : 0, HB_DISK_USED ), -1, 0 );
}

HB_FUNC( DISKFREE )
{
   hb_retnlen( hb_fsDiskSpace( ISNUM( 1 ) ? hb_parni( 1 ) : 0, HB_DISK_FREE ), -1, 0 );
}

HB_FUNC( DISKFULL )
{
   hb_retnlen( hb_fsDiskSpace( ISNUM( 1 ) ? hb_parni( 1 ) : 0, HB_DISK_TOTAL ), -1, 0 );
}

