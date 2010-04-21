/*
 * $Id$
 */

This folder contains compression related files including:
 * zip file support based on minizip library (http://www.winimage.com/zLibDll/minizip.html)
   including:
   * minizip version 1.1 source files

Some small changes (to fix compile time warning and errors) are applied to original
source of minizip 1.1:

  * harbour/contrib/hbzlib/zip.c
    * added forward definitions of allocate_new_datablock(),
      free_datablock(), init_linkedlist(), add_data_in_datablock(),
      ziplocal_TmzDateToDosDate()
    * pacified warnings of unused args dosDate and crcForCrypting
    * fixed 2 warnings: assigned value is not used. See, TOFIX
      comment for one of the fixes
    * fixed BCC warning "function call with no prototype" by changing
           local int zipFlushWriteBuffer(zi)
              zip_internal* zi;
           {
      to
           local int zipFlushWriteBuffer(zip_internal* zi)
           {
      I expected this code be equivavlent! ??? :/

  * harbour/contrib/hbzlib/unzip.c
    * added forward definitions of strcmpcasenosensitive_internal(),
      unzlocal_DosDateToTmuDate(), unzlocal_CheckCurrentFileCoherencyHeader()
    * fixed 8 warnings: assigned value is not used. See, TOFIX
      comment for one of the fixes

  * harbour/contrib/hbzlib/ioapi.c
    * pacified 7 warnings: unused args opaque
    * fixed warning: assigned value is not used
