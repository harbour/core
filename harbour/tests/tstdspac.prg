/*

 $Id$

 tstdspac

 This program demonstrates that the 4 diskspace related functions work
 correctly for disks of any size.

 Certain os's may allow limits to the amount of disk space available to
 a user.  If that is the case, you should see a difference between
 the return value of DiskSpace() and DiskFree().

 Currently, Disk quota's are only implimented for NT.

 NOTE: Unlike Clipper, these functions return a floating point number!

 Written by Paul Tucker {ptucker@sympatico.ca>
 www - http://harbour-project.org

 This test program placed in the public domain
*/

#include "fileio.ch"

proc main( cDisk )
  if empty( cDisk )
      cDisk := "0"
  Endif
  cDisk := Val( cDisk )

? "Bytes available on disk: " + Transform( diskspace(cDisk, HB_DISK_FREE ),"999,999,999,999")
? "Bytes available for use: " + Transform( diskspace(cDisk, HB_DISK_AVAIL ),"999,999,999,999")
? "             Bytes used: " + Transform( diskspace(cDisk, HB_DISK_USED ),"999,999,999,999")
? " Total bytes on disk "+PadL(cDisk,2)+": " + Transform( diskspace(cDisk, HB_DISK_TOTAL ),"999,999,999,999")

  return
