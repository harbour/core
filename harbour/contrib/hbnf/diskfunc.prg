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

#include "ftint86.ch"

#define DRVTABLE "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#ifdef FT_TEST
  PROCEDURE Main( cDrv )

     QOut("Disk size:   " + str( FT_DSKSIZE() ) )
     QOut("Free bytes:  " + str( FT_DSKFREE() ) )

  RETURN
#endif

FUNCTION FT_DSKSIZE( cDrive )
   local nDrive
   nDrive := iif( cDrive == NIL, 0, at( upper(cDrive), DRVTABLE ) )

Return DISKSPACE(nDrive,3)

FUNCTION FT_DSKFREE( cDrive )
   local nDrive
   nDrive := iif( cDrive == NIL, 0, at( upper(cDrive), DRVTABLE ) )

RETURN    DISKSPACE(nDrive,1)
