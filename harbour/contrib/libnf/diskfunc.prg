/*
 * File......: DISKFUNC.PRG
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
  FUNCTION MAIN( cDrv )

     QOut("Disk size:   " + str( FT_DSKSIZE() ) )
     QOut("Free bytes:  " + str( FT_DSKFREE() ) )

  return ( nil )
#endif

/*  $DOC$
 *  $FUNCNAME$
 *     FT_DSKSIZE()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Return the maximum capacity of a fixed disk
 *  $SYNTAX$
 *     FT_DSKSIZE( [ <cDrive> ] ) -> nMaxCapacity
 *  $ARGUMENTS$
 *     <cDrive> is the fixed disk to query. If no drive is sent, the
 *     operation will be performed on the default drive. Send without
 *     the ":".
 *  $RETURNS$
 *     An integer representing the maximum disk capacity in bytes.
 *  $DESCRIPTION$
 *     Function utilizing FT_INT86() to return Maximum Disk Size.
 *     Uses FT_INT86() through the internal function _ftDiskInfo().
 *  $EXAMPLES$
 *     ? FT_DSKSIZE()      // Maximum capacity for default drive
 *     ? FT_DSKSIZE( "D" ) // Maximum capacity for Drive D:
 *  $END$
 */

FUNCTION FT_DSKSIZE( cDrive )
   local nDrive
   nDrive := if( cDrive == NIL, 0, at( upper(cDrive), DRVTABLE ) )

Return DISKSPACE(nDrive,3)


/*  $DOC$
 *  $FUNCNAME$
 *     FT_DSKFREE()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Return the amount of available disk space
 *  $SYNTAX$
 *     FT_DSKFREE( [ <cDrive> ] ) -> nSpaceAvail
 *  $ARGUMENTS$
 *     <cDrive> is the fixed disk to query. If no parameter is passed
 *     the operation will be performed on the default drive.  Do not
 *     include the ":".
 *  $RETURNS$
 *     Integer representing the available disk space in bytes.
 *  $DESCRIPTION$
 *     Function to return the available space on the passed
 *     drive letter or the default drive if no drive is passed.
 *
 *     Uses FT_INT86() through the internal function _ftDiskInfo().
 *  $EXAMPLES$
 *     ? FT_DSKFREE()  // Returns free space on default drive.
 *  $END$
 */


FUNCTION FT_DSKFREE( cDrive )
   local nDrive
   nDrive := if( cDrive == NIL, 0, at( upper(cDrive), DRVTABLE ) )


RETURN    DISKSPACE(nDrive,1)

