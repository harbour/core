/*
 * $Id$
 */


This folder contains compression related files including:
 * zip file support based on minizip library (http://www.winimage.com/zLibDll/minizip.html)
   including:
   * minizip version 1.01e source files 
   * wrapper functions for minizip
   * some additionl functions to provide a higher level API for zip files

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


Harbour functions to mange ZIP files:
=====================================

HB_ZipOpen( cFileName, [ iMode = HB_ZIP_CREATE ],
            [ @cGlobalComment ] ) --> hZip
HB_ZipClose( hZip, [ cGlobalComment ] ) --> nError
HB_ZipFileCreate( hZip, cZipName, dDate, cTime,
                  nInternalAttr, nExternalAttr,
                  [ nMethod = HB_ZLIB_METHOD_DEFLATE ], 
                  [ nLevel = HB_ZLIB_COMPRESSION_DEFAULT ], 
                  [ cPassword, ulFileCRC32 ], [ cComment ] ) --> nError
HB_ZipFileWrite( hZip, cData [, nLen ] ) --> nError
HB_ZipFileClose( hZip ) --> nError
HB_ZipStoreFile( hZip, cFileName, [ cZipName ], ;
                 [ cPassword ], [ cComment ] ) --> nError
HB_zipFileCRC32( cFileName ) --> nError


HB_UnzipOpen( cFileName ) --> hUnzip
HB_UnzipClose( hUnzip ) --> nError
HB_UnzipGlobalInfo( hUnzip, @nEntries, @cGlobalComment ) --> nError
HB_UnzipFileFirst( hUnzip ) --> nError
HB_UnzipFileNext( hUnzip ) --> nError
HB_UnzipFilePos( hUnzip ) --> nPosition
HB_UnzipFileGoto( hUnzip, nPosition ) --> nError
HB_UnzipFileInfo( hUnzip, @cZipName, @dDate, @cTime,
                  @nInternalAttr, @nExternalAttr,
                  @nMethod, @nSize, @nCompressedSize,
                  @lCrypted, @cComment ) --> nError
HB_UnzipFileOpen( hUnzip, [ cPassword ] ) --> nError
HB_UnzipFileRead( hUnzip, @cBuf [, nLen ] ) --> nRead
HB_UnzipFileClose( hUnzip ) --> nError
HB_UnzipExtractCurrentFile( hZip, [ cFileName ], [ cPassword ] ) --> nError


HB_ZipDeleteFile( cZipFile, cFileMask ) --> nError
