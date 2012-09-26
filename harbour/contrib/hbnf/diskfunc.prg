/*
 * $Id$
 */

/*
 * File......: diskfunc.prg
 * Author....: Robert A. DiFalco
 * CIS ID....: ?
 *
 * This is an original work by Robert A. DiFalco and is placed in
 * the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:20   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 17:49:28   GLENN
 * Documentation format change (minor).
 * Added work around suggested by a number of Nanforum users; sometimes
 * _ftDiskFunc() would return negative numbers on large drives.
 *
 *    Rev 1.0   01 Apr 1991 01:01:12   GLENN
 * Nanforum Toolkit
 *
 */

#include "fileio.ch"

#define DRVTABLE "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#ifdef FT_TEST

PROCEDURE Main( cDrv )

   QOut( "Disk size:   " + Str( FT_DSKSIZE() ) )
   QOut( "Free bytes:  " + Str( FT_DSKFREE() ) )

   RETURN

#endif

FUNCTION FT_DSKSIZE( cDrive )
   RETURN DiskSpace( iif( cDrive == NIL, 0, At( Upper( cDrive ), DRVTABLE ) ), HB_DISK_TOTAL )

FUNCTION FT_DSKFREE( cDrive )
   RETURN DiskSpace( iif( cDrive == NIL, 0, At( Upper( cDrive ), DRVTABLE ) ), HB_DISK_FREE )
