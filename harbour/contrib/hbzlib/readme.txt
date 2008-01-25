/*
 * $Id$
 */


This folder contains compression related files including:
 * zlib library (http://www.zlib.net/) wrapper functions
 * zip file support based on minizip library (http://www.winimage.com/zLibDll/minizip.html)
   including:
   * minizip version 1.01e source files 
   * wrapper functions for minizip
   * some additionl functions to provide a higher level API for zip files

The sources of zlib itself is not included. You'll find it in the system under linux.
For windows, please, download dll file containig compiled zlib library and make import 
library for the compiler you are using. If you want a static library try to locate it on 
the net, ex., you can find it for several compiler on http://libharu.sourceforge.net/


Some small changes (to fix compile time warning and errors) are applied to original 
source of minizip 1.01e:

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

