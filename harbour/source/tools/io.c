/*
 * $Id$
 */

#include "extend.h"

#ifdef DOS
#include <dos.h>
#include <dir.h>
#include <bios.h>
#endif

HARBOUR HB_DISKUSED( void )
{
#ifdef DOS
   struct diskfree_t disk;
   long bytsfree, bytsfull;

   _dos_getdiskfree( 0, &disk );

   bytsfree = ( long ) disk.avail_clusters *
              ( long ) disk.sectors_per_cluster *
              ( long ) disk.bytes_per_sector;
   bytsfull = ( long ) disk.total_clusters *
              ( long ) disk.sectors_per_cluster *
              ( long ) disk.bytes_per_sector;

   hb_retnl( bytsfull - bytsfree );
#else
   hb_retnl( 0 );
#endif
}

HARBOUR HB_DISKFULL( void )
{
#ifdef DOS
   struct diskfree_t disk;

   _dos_getdiskfree( 0, &disk );

   hb_retnl( ( long ) disk.total_clusters *
             ( long ) disk.sectors_per_cluster *
             ( long ) disk.bytes_per_sector );
#else
   hb_retnl( 0 );
#endif
}
