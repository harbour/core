/*
 * $Id$
 */

#include "extend.h"

#ifdef DOS
#include <dos.h>
#include <dir.h>
#include <bios.h>
#endif

/*  $DOC$
 *  $FUNCNAME$
 *     CD()
 *  $CATEGORY$
 *     DOS
 *  $ONELINER$
 *  $SYNTAX$
 *     CD(<NDIR>)
 *  $ARGUMENTS$
 *     <NDIR> DIR TO BE CHANGED
 *  $RETURNS$
 *     .T. IF SUCEFUL
 *     .F. IF NOT
 *
 *  $DESCRIPTION$
 *     CHANGE THE CURRENT DIRECTORY
 *  $EXAMPLES$
 *     IF CD("OLA")
 *        RETURN(.T.)
 *     ELSE
 *        RETURN(.F.)
 *     ENDIF
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *     extend.h dos.h dir.h bios.h
 *  $END$
 */

HARBOUR HB_CD( void )
{
#ifdef __DOS__
   hb_retni( ISCHAR( 1 ) ? chdir( hb_parc( 1 ) ) : 0 );
#else
   hb_retni( 0 );
#endif
}

/*  $DOC$
 *  $FUNCNAME$
 *     MD()
 *  $CATEGORY$
 *     DOS
 *  $ONELINER$
 *
 *  $SYNTAX$
 *     MD(<NDIR>)
 *  $ARGUMENTS$
 *    <NDIR> DIRECTORY TO BE CREATED
 *  $RETURNS$
 *    .T. IF SUCEFUL
 *    .F. IF NOT
 *
 *  $DESCRIPTION$
 *    CREATE A  DIRECTORY
 *  $EXAMPLES$
 *  IF MD("OLA")
 *      RETURN(.T.)
 *  ELSE
 *      RETURN(.F.)
 *  ENDIF
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *     extend.h dos.h dir.h bios.h
 *  $END$
 */

HARBOUR HB_MD(void)
{
#ifdef __DOS__
   hb_retni( ISCHAR( 1 ) ? mkdir( hb_parc( 1 ) ) : 0 );
#else
   hb_retni( 0 );
#endif
}

/*  $DOC$
 *  $FUNCNAME$
 *     RD()
 *  $CATEGORY$
 *     DOS
 *  $ONELINER$
 *
 *  $SYNTAX$
 *     RD(<NDIR>)
 *  $ARGUMENTS$
 *    <NDIR> DIR TO BE DELETED
 *  $RETURNS$
 *    .T. IF SUCEFUL
 *    .F. IF NOT
 *
 *  $DESCRIPTION$
 *    REMOVE A  DIRECTORY
 *  $EXAMPLES$
 *  IF RD("OLA")
 *      RETURN(.T.)
 *  ELSE
 *      RETURN(.F.)
 *  ENDIF
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *     extend.h dos.h dir.h bios.h
 *  $END$
 */

HARBOUR HB_RD( void )
{
   /* TOFIX: chdir() is wrong here */
#ifdef __DOS__
   hb_retni( ISCHAR( 1 ) ? chdir( hb_parc( 1 ) ) : 0 );
#else
   hb_retni( 0 );
#endif
}

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

HARBOUR HB_DISKFREE( void )
{
#ifdef DOS
   struct diskfree_t disk;

   _dos_getdiskfree( 0, &disk );

   hb_retnl( ( long ) disk.avail_clusters *
             ( long ) disk.sectors_per_cluster *
             ( long ) disk.bytes_per_sector );
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
