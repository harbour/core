/*
 * $Id$
 */


This folder contains compression related files including:
 * zip file support based on minizip library (http://www.winimage.com/zLibDll/minizip.html)
   including:
   * wrapper functions for minizip
   * some additionl functions to provide a higher level API for zip files


Harbour functions to mange ZIP files:
=====================================

HB_ZipOpen( cFileName, [ iMode = HB_ZIP_CREATE ],
            [ @cGlobalComment ] ) --> hZip
HB_ZipClose( hZip, [ cGlobalComment ] ) --> nError
HB_ZipFileCreate( hZip, cZipName, tDateTime, cTime,
                  nInternalAttr, nExternalAttr,
                  [ nMethod = HB_ZLIB_METHOD_DEFLATE ],
                  [ nLevel = HB_ZLIB_COMPRESSION_DEFAULT ],
                  [ cPassword, ulFileCRC32 ], [ cComment ] ) --> nError
HB_ZipFileWrite( hZip, cData [, nLen ] ) --> nError
HB_ZipFileClose( hZip ) --> nError
HB_ZipStoreFile( hZip, cFileName, [ cZipName ], ;
                 [ cPassword ], [ cComment ] ) --> nError
HB_ZipStoreFileHandle( hZip, fhnd, cZipName, ;
                 [ cPassword ], [ cComment ] ) --> nError
HB_zipFileCRC32( cFileName ) --> nError


HB_UnzipOpen( cFileName ) --> hUnzip
HB_UnzipClose( hUnzip ) --> nError
HB_UnzipGlobalInfo( hUnzip, @nEntries, @cGlobalComment ) --> nError
HB_UnzipFileFirst( hUnzip ) --> nError
HB_UnzipFileNext( hUnzip ) --> nError
HB_UnzipFilePos( hUnzip ) --> nPosition
HB_UnzipFileGoto( hUnzip, nPosition ) --> nError
HB_UnzipFileInfo( hUnzip, @cZipName, @tDateTime, @cTime,
                  @nInternalAttr, @nExternalAttr,
                  @nMethod, @nSize, @nCompressedSize,
                  @lCrypted, @cComment ) --> nError
HB_UnzipFileOpen( hUnzip, [ cPassword ] ) --> nError
HB_UnzipFileRead( hUnzip, @cBuf [, nLen ] ) --> nRead
HB_UnzipFileClose( hUnzip ) --> nError
HB_UnzipExtractCurrentFile( hUnzip, [ cFileName ], [ cPassword ] ) --> nError
HB_UnzipExtractCurrentFileToHandle( hZip, fhnd, [ cPassword ] ) --> nError


HB_ZipDeleteFile( cZipFile, cFileMask ) --> nError
