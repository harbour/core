/*
 * $Id$
 */

#include "hbapi.h"

#if defined(HB_OS_DOS)
   #include <dos.h>
   #include <dir.h>
   #include <bios.h>
#endif

HB_FUNC( CD )
{
#if defined(HB_OS_DOS)
   hb_retni( ISCHAR( 1 ) ? chdir( hb_parc( 1 ) ) : 0 );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( MD )
{
#if defined(HB_OS_DOS)
   hb_retni( ISCHAR( 1 ) ? mkdir( hb_parc( 1 ) ) : 0 );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( RD )
{
#if defined(HB_OS_DOS)
   hb_retni( ISCHAR( 1 ) ? rmdir( hb_parc( 1 ) ) : 0 );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( DISKUSED )
{
   hb_retnd( hb_DiskSpace( ISNUM( 1 ) ? hb_parni( 1 ) : 0, HB_DISK_USED ));
}


HB_FUNC( DISKFREE )
{
   hb_retnd( hb_DiskSpace( ISNUM( 1 ) ? hb_parni( 1 ) : 0, HB_DISK_FREE ));
}


HB_FUNC( DISKFULL )
{
   hb_retnd( hb_DiskSpace( ISNUM( 1 ) ? hb_parni( 1 ) : 0, HB_DISK_TOTAL ));
}
