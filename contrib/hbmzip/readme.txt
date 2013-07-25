This folder contains compression related files including:
 * zip file support based on minizip library (http://www.winimage.com/zLibDll/minizip.html)
   including:
   * wrapper functions for minizip
   * some additionl functions to provide a higher level API for zip files


Harbour functions to mange ZIP files:
=====================================

hb_zipOpen( cFileName, [ iMode = HB_ZIP_CREATE ],
            [ @cGlobalComment ] ) --> hZip
hb_zipClose( hZip, [ cGlobalComment ] ) --> nError
hb_zipFileCreate( hZip, cZipName, tDateTime, cTime,
                  nInternalAttr, nExternalAttr,
                  [ nMethod = HB_ZLIB_METHOD_DEFLATE ],
                  [ nLevel = HB_ZLIB_COMPRESSION_DEFAULT ],
                  [ cPassword, ulFileCRC32 ], [ cComment ] ) --> nError
hb_zipFileWrite( hZip, cData [, nLen ] ) --> nError
hb_zipFileClose( hZip ) --> nError
hb_zipStoreFile( hZip, cFileName, [ cZipName ], ;
                 [ cPassword ], [ cComment ] ) --> nError
hb_zipStoreFileHandle( hZip, fhnd, cZipName, ;
                 [ cPassword ], [ cComment ] ) --> nError
hb_zipFileCRC32( cFileName ) --> nError


hb_unzipOpen( cFileName ) --> hUnzip
hb_unzipClose( hUnzip ) --> nError
hb_unzipGlobalInfo( hUnzip, @nEntries, @cGlobalComment ) --> nError
hb_unzipFileFirst( hUnzip ) --> nError
hb_unzipFileNext( hUnzip ) --> nError
hb_unzipFilePos( hUnzip ) --> nPosition
hb_unzipFileGoto( hUnzip, nPosition ) --> nError
hb_unzipFileInfo( hUnzip, @cZipName, @tDateTime, @cTime,
                  @nInternalAttr, @nExternalAttr,
                  @nMethod, @nSize, @nCompressedSize,
                  @lCrypted, @cComment ) --> nError
hb_unzipFileOpen( hUnzip, [ cPassword ] ) --> nError
hb_unzipFileRead( hUnzip, @cBuf [, nLen ] ) --> nRead
hb_unzipFileClose( hUnzip ) --> nError
hb_unzipExtractCurrentFile( hUnzip, [ cFileName ], [ cPassword ] ) --> nError
hb_unzipExtractCurrentFileToHandle( hZip, fhnd, [ cPassword ] ) --> nError


hb_zipDeleteFile( cZipFile, cFileMask ) --> nError
