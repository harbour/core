/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ZipArchive interface compatibility implementation.
 *
 * Copyright 2008 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2008 Toninho (toninhofwi yahoo.com.br)
 * Copyright 2000-2001 Luiz Rafael Culik <culik@sl.conex.net>
 *   (original ZipArchive interface, docs)
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2,  or ( at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not,  write to
 * the Free Software Foundation,  Inc.,  59 Temple Place,  Suite 330,
 * Boston,  MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
 *
 * As a special exception,  the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that,  if you link the Harbour libraries with other
 * files to produce an executable,  this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour,  as the General Public License permits,  the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files,  you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour,  it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that,  delete this exception notice.
 *
 */

#include "common.ch"
#include "directry.ch"
#include "fileio.ch"

THREAD STATIC t_nReadBuffer := 32768
THREAD STATIC t_cComment
THREAD STATIC t_lReadOnly := .F.

PROCEDURE SetZipReadOnly( lReadOnly )

   DEFAULT lReadOnly TO .F.

   t_lReadOnly := lReadOnly

   /* TODO: Implement. */

   RETURN

/* $DOC$
 * $FUNCNAME$
 *     hb_SetZipComment()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Set an Zip archive Comment
 * $SYNTAX$
 *     hb_SetZipComment( <cComment> ) --> NIL
 * $ARGUMENTS$
 *     <cComment>   Comment to add to the zip archive
 * $RETURNS$
 *     <NIL> this function always return NIL
 * $DESCRIPTION$
 *     This function stored an global comment to an zip archive.
 *     It should be called before any of the compression functions.
 * $EXAMPLES$
 *     hb_SetZipComment( "This is an Test" )
 *     hb_ZipFile( "test.zip", { "\windows\ios.ini", "\windows\win.ini" } )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparc
 * $END$
 */
PROCEDURE hb_SetZipComment( cComment )

   IF cComment == NIL .OR. ISCHARACTER( cComment )
      t_cComment := cComment
   ENDIF

   RETURN

/* $DOC$
 * $FUNCNAME$
 *     hb_GetZipComment()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Return the comment of an zip file
 * $SYNTAX$
 *     hb_GetZipComment( <szFile> ) --> <szComment>
 * $ARGUMENTS$
 *     <szFile>  File to get the comment from
 * $RETURNS$
 *     <szComment>  The comment that was stored in <szFile>
 * $DESCRIPTION$
 *     This function receives a valid zip file name as parameter,
 *     and returns the global comment stored within.
 * $EXAMPLES$
 *     ? "The comment in test.zip is ", hb_GetZipComment( "test.zip" )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
FUNCTION hb_GetZipComment( cFileName )
   LOCAL hUnzip
   LOCAL cComment
   LOCAL cExt

   IF Set( _SET_DEFEXTENSIONS )
      hb_FNameSplit( cFileName, NIL, NIL, @cExt )
      IF Empty( cExt )
         cFileName += ".zip"
      ENDIF
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      hb_UnzipGlobalInfo( hUnzip, NIL, @cComment )
      hb_UnzipClose( hUnzip )
   ELSE
      cComment := ""
   ENDIF

   RETURN cComment

FUNCTION hb_GetFileCount( cFileName )
   LOCAL hUnzip
   LOCAL nEntries
   LOCAL cExt

   IF Set( _SET_DEFEXTENSIONS )
      hb_FNameSplit( cFileName, NIL, NIL, @cExt )
      IF Empty( cExt )
         cFileName += ".zip"
      ENDIF
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      hb_UnzipGlobalInfo( hUnzip, @nEntries, NIL )
      hb_UnzipClose( hUnzip )
   ELSE
      nEntries := 0
   ENDIF

   RETURN nEntries

FUNCTION hb_ZipWithPassword( cFileName )
   LOCAL lCrypted := .F.
   LOCAL hUnzip
   LOCAL cExt

   IF Set( _SET_DEFEXTENSIONS )
      hb_FNameSplit( cFileName, NIL, NIL, @cExt )
      IF Empty( cExt )
         cFileName += ".zip"
      ENDIF
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )

      IF hb_UnzipFileFirst( hUnzip ) == 0
         hb_UnzipFileInfo( hUnzip, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, @lCrypted )
      ENDIF

      hb_UnzipClose( hUnzip )
   ENDIF

   RETURN lCrypted

FUNCTION hb_GetFilesInZip( cFileName, lVerbose )

   LOCAL hUnzip
   LOCAL nErr
   LOCAL cExt

   LOCAL dDate
   LOCAL cTime
   LOCAL nSize
   LOCAL nCompSize
   LOCAL nInternalAttr
   LOCAL nMethod
   LOCAL lCrypted
   LOCAL cComment
   LOCAL nRatio

   LOCAL aFiles := {}

   IF Set( _SET_DEFEXTENSIONS )
      hb_FNameSplit( cFileName, NIL, NIL, @cExt )
      IF Empty( cExt )
         cFileName += ".zip"
      ENDIF
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )

      DEFAULT lVerbose TO .F.

      nErr := hb_UnzipFileFirst( hUnzip )
      DO WHILE nErr == 0

         hb_UnzipFileInfo( hUnzip, @cFileName, @dDate, @cTime, @nInternalAttr, NIL, @nMethod, @nSize, @nCompSize, @lCrypted, @cComment )

         IF lVerbose

            IF nSize > 0
               nRatio := 100 - ( ( nCompSize * 100 ) / nSize )
               IF nRatio < 0
                  nRatio := 0
               ENDIF
            ELSE
               nRatio := 0
            ENDIF

            /* TOFIX: Original hbziparch has nMethod as string: Unknown, Stored, DeflatN, DeflatX, DeflatF. */
            /* TOFIX: Original hbziparch has attributes as string. */
            /* TOFIX: Original hbziparch has crc32 filled with a hexadecimal value. */
            AAdd( aFiles, { cFileName, nSize, nMethod, nCompSize, nRatio, dDate, cTime, "" /* crc32 */, nInternalAttr /* cAttr */, lCrypted, cComment } )
         ELSE
            AAdd( aFiles, cFileName )
         ENDIF

         nErr := hb_UnzipFileNext( hUnzip )
      ENDDO

      hb_UnzipClose( hUnzip )
   ENDIF

   RETURN aFiles

/* $DOC$
 * $FUNCNAME$
 *     hb_ZipTestPK()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Test pkSpanned zip files
 * $SYNTAX$
 *     hb_ZipTestPK( <cFile> ) --> <nReturnCode>
 * $ARGUMENTS$
 *     <cFile>  File to be tested.
 * $RETURNS$
 *     <nReturn> A code that tells if the current disk is the last of a
 *     pkSpanned disk set.
 * $DESCRIPTION$
 *     This function tests if the disk inserted is the last disk of an backup
 *     set or not.
 *     It will return the follow return code when an error is found
 *
 *     <table>
 *     Error code     Meaning
 *     114            Incorrect Disk
 *     103            No Call back was set with hb_ZipTestPK()
 *     </table>
 *
 *     Call this function to determine if the disk inserted is the correct
 *     one before any other function.
 * $EXAMPLES$
 *     IF hb_ZipTestPK( "A:\test22.zip" ) == 114
 *        ? "Invalid Diskette"
 *     ENDIF
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
FUNCTION hb_ZipTestPK( cFileName )

   HB_SYMBOL_UNUSED( cFileName )

   /* NOTE: Spanning not supported. */

   RETURN 0

/* $DOC$
 * $FUNCNAME$
 *     hb_SetDiskZip()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Set an codeblock for disk changes
 * $SYNTAX$
 *     hb_SetDiskZip( <bBlock> ) --> .T.
 * $ARGUMENTS$
 *     <bBlock> an Code block that contains an function that will be performed
 *     when the need of changing disk are need.
 * $RETURNS$
 *     It always returns True
 * $DESCRIPTION$
 *     This function will set an codeblock that will be evaluated every time
 *     that an changedisk event is necessary. <bBlock> receives nDisk as a
 *     code block param that corresponds to the diskette number to be processed.
 *
 *     Set this function before opening archives that are in removable media.
 *     This block will be released, when the caller finish it job.
 * $EXAMPLES$
 *     hb_SetDiskZip( {| nDisk | Alert( "Please insert disk no " + Str( nDisk, 3 ) ) } )
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
FUNCTION hb_SetDiskZip( bBlock )

   HB_SYMBOL_UNUSED( bBlock )

   /* NOTE: Spanning not supported. */

   RETURN .F.

FUNCTION TransferFromZip( cZipSrc, cZipDst, aFiles )

   HB_SYMBOL_UNUSED( cZipSrc )
   HB_SYMBOL_UNUSED( cZipDst )
   HB_SYMBOL_UNUSED( aFiles )

   /* TODO: Implement. */

   RETURN .F.

/* $DOC$
 * $FUNCNAME$
 *     hb_SetBuffer()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *
 * $SYNTAX$
 *     hb_SetBuffer( [<nWriteBuffer>], [<nExtractBuffer>], [<nReadBuffer>] ) --> NIL
 * $ARGUMENTS$
 *     <nWriteBuffer>   The size of the write buffer.
 *
 *     <nExtractBuffer> The size of the extract buffer.
 *
 *     <nReadBuffer>    The size of the read buffer.
 * $RETURNS$
 *     <NIL>            This function always returns NIL.
 * $DESCRIPTION$
 *     This function set the size of the internal buffers for write/extract/read
 *     operation.
 *
 *     If the size of the buffer is smaller then the default, the function
 *     will automatically use the default values, which are 65535/16384/32768
 *     respectively.
 *
 *     This function be called before any of the compression/decompression
 *     functions.
 * $EXAMPLES$
 *     hb_SetBuffer( 100000, 115214, 65242 )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
PROCEDURE hb_SetBuffer( nWriteBuffer, nExtractBuffer, nReadBuffer )

   HB_SYMBOL_UNUSED( nWriteBuffer )
   HB_SYMBOL_UNUSED( nExtractBuffer )

   IF !Empty( nReadBuffer )
      t_nReadBuffer := Min( nReadBuffer, 32768 )
   ENDIF

   RETURN

/*
 * $DOC$
 * $FUNCNAME$
 *     hb_ZipFileByTDSpan()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Create a zip file
 * $SYNTAX$
 *     hb_ZipFileByTDSpan( <cFile> ,<cFileToCompress> | <aFiles>, <nLevel>,
 *     <bBlock>, <lOverWrite>, <cPassword>, <iSize>, <lWithPath>, <lWithDrive>,
 *     <pFileProgress>) --> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file
 *
 *     <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *     can be used
 *         _or_
 *     <aFiles>  An array containing files to compress, Drive and/or path
 *     can be used
 *
 *     <nLevel>  Compression level ranging from 0 to 9
 *
 *     <bBlock>  Code block to execute while compressing
 *
 *     <lOverWrite>  Toggle to overwrite the file if exists
 *
 *     <cPassword> Password to encrypt the files
 *
 *     <iSize> Size of the archive, in bytes. Default is 1457664 bytes
 *
 *     <lWithPath> Toggle to store the path or not
 *
 *     <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *     This function creates a zip file named <cFile>. If the extension
 *     is omitted, .zip will be assumed. If the second parameter is a
 *     character string, this file will be added to the zip file. If the
 *     second parameter is an array, all file names contained in <aFiles>
 *     will be compressed.
 *
 *     If <nLevel> is used, it determines the compression type where 0 means
 *     no compression and 9 means best compression.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     If <lOverWrite> is used, it toggles to overwrite or not the existing
 *     file. Default is to overwrite the file, otherwise if <lOverWrite> is
 *     false the new files are added to the <cFile>.
 *
 *     If <lWithPath> is used, it tells thats the path should also be stored
 *     with the file name. Default is false.
 *
 *     If <lWithDrive> is used, it tells thats the Drive and path should also
 *     be stored with the file name. Default is false.
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {| nPos, nTotal | GaugeUpdate( aGauge1, nPos / nTotal ) }
 * $EXAMPLES$
 *     PROCEDURE Main()
 *
 *     IF hb_ZipFileByTDSpan( "test.zip", "test.prg" )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     IF hb_ZipFileByTDSpan( "test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     IF hb_ZipFileByTDSpan( "test2.zip", { "test.prg", "C:\windows\win.ini" }, 9, {| nPos, cFile | QOut( cFile ) }, "hello",, 521421 )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     aFiles := { "test.prg", "C:\windows\win.ini" }
 *     nLen   := Len( aFiles )
 *     aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "." )
 *     GaugeDisplay( aGauge )
 *     hb_ZipFileByTDSpan( "test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos / nLen) },, "hello",, 6585452 )
 *     RETURN
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
FUNCTION hb_ZipFileByTDSpan( cFileName, aFileToCompress, nLevel, bUpdate, lOverwrite, cPassword, nSpanSize, lWithPath, lWithDrive, bProgress, lFullPath, acExclude )

   HB_SYMBOL_UNUSED( nSpanSize )

   /* NOTE: Spanning not supported. */

   RETURN hb_ZipFile( cFileName, aFileToCompress, nLevel, bUpdate, lOverwrite, cPassword, lWithPath, lWithDrive, bProgress, lFullPath, acExclude )

/*
 * $DOC$
 * $FUNCNAME$
 *     hb_ZipFileByPKSpan()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Create a zip file on removable media
 * $SYNTAX$
 *     hb_ZipFileByPKSpan( <cFile>, <cFileToCompress> | <aFiles>, <nLevel>,
 *     <bBlock>, <lOverWrite>, <cPassword>, <lWithPath>, <lWithDrive>,
 *     <pFileProgress> ) --> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file
 *
 *     <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *     can be used
 *         _or_
 *     <aFiles>  An array containing files to compress, Drive and/or path
 *     can be used
 *
 *     <nLevel>  Compression level ranging from 0 to 9
 *
 *     <bBlock>  Code block to execute while compressing
 *
 *     <lOverWrite>  Toggle to overwrite the file if exists
 *
 *     <cPassword> Password to encrypt the files
 *
 *     <lWithPath> Toggle to store the path or not
 *
 *     <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *     This function creates a zip file named <cFile>. If the extension
 *     is omitted, .zip will be assumed. If the second parameter is a
 *     character string, this file will be added to the zip file. If the
 *     second parameter is an array, all file names contained in <aFiles>
 *     will be compressed.  Also, the use of this function is for creating
 *     backup in removable media like an floppy drive/zip drive.
 *
 *     If <nLevel> is used, it determines the compression type where 0 means
 *     no compression and 9 means best compression.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     If <lOverWrite> is used , it toggles to overwrite or not the existing
 *     file. Default is to overwrite the file, otherwise if <lOverWrite> is false
 *     the new files are added to the <cFile>.
 *
 *     If <cPassword> is used, all files that are added to the archive are encrypted
 *     with the password.
 *
 *     If <lWithPath> is used, it tells thats the path should also be stored with
 *     the file name. Default is false.
 *
 *     If <lWithDrive> is used, it tells thats the Drive and path should also be stored
 *     with the file name. Default is false.
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {| nPos, nTotal | GaugeUpdate( aGauge1, nPos / nTotal ) }
 *
 *     Before calling this function, Set an Changedisk codeblock by calling
 *     the hb_SetDiskZip().
 * $EXAMPLES$
 *     PROCEDURE Main()
 *
 *     hb_SetDiskZip( {| nDisk | Alert( "Please insert disk no " + Str( nDisk, 3 ) ) } )
 *
 *     IF hb_ZipFileByPKSpan( "A:\test.zip", "test.prg" )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     IF hb_ZipFileByPKSpan( "A:\test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     IF hb_ZipFileByPKSpan( "test2.zip", { "test.prg", "C:\windows\win.ini" }, 9, {| nPos, cFile | QOut( cFile ) } )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     aFiles := { "test.prg", "C:\windows\win.ini" }
 *     nLen   := Len( aFiles )
 *     aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "." )
 *     GaugeDisplay( aGauge )
 *     // assuming F: is a Zip Drive
 *     hb_ZipFileByPKSpan( "F:\test33.zip", aFiles, 9, {| cFile, nPos | GaugeUpdate( aGauge, nPos / nLen ) },, "hello" )
 *     RETURN
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
FUNCTION hb_ZipFileByPKSpan( ... )

   /* NOTE: Spanning not supported. */

   RETURN hb_ZipFile( ... )

/*
 * $DOC$
 * $FUNCNAME$
 *     hb_ZipFile()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Create a zip file
 * $SYNTAX$
 *     hb_ZipFile( <cFile>, <cFileToCompress> | <aFiles>, <nLevel>,
 *     <bBlock>, <lOverWrite>, <cPassword>, <lWithPath>, <lWithDrive>,
 *     <pFileProgress> ) --> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file to create
 *
 *     <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *     can be used
 *        _or_
 *     <aFiles>  An array containing files to compress, Drive and/or path
 *     can be used
 *
 *     <nLevel>  Compression level ranging from 0 to 9
 *
 *     <bBlock>  Code block to execute while compressing
 *
 *     <lOverWrite>  Toggle to overwrite the file if exists
 *
 *     <cPassword> Password to encrypt the files
 *
 *     <lWithPath> Toggle to store the path or not
 *
 *     <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *     This function creates a zip file named <cFile>. If the extension
 *     is omitted, .zip will be assumed. If the second parameter is a
 *     character string, this file will be added to the zip file. If the
 *     second parameter is an array, all file names contained in <aFiles>
 *     will be compressed.
 *
 *     If <nLevel> is used, it determines the compression type where 0 means
 *     no compression and 9 means best compression.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     If <lOverWrite> is used, it toggles to overwrite or not the existing
 *     file. Default is to overwrite the file,otherwise if <lOverWrite> is false
 *     the new files are added to the <cFile>.
 *
 *     If <cPassword> is used, all files that are added to the archive are encrypted
 *     with the password.
 *
 *     If <lWithPath> is used, it tells  the path should also be stored with
 *     the file name. Default is false.
 *
 *     If <lWithDrive> is used, it tells thats the Drive and path should also be stored
 *     with the file name. Default is false.
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {| nPos, nTotal | GaugeUpdate( aGauge1, nPos / nTotal ) }
 *
 * $EXAMPLES$
 *     PROCEDURE Main()
 *
 *     IF hb_ZipFile( "test.zip", "test.prg" )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     IF hb_ZipFile( "test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     IF hb_ZipFile( "test2.zip", { "test.prg", "C:\windows\win.ini" }, 9, {| cFile, nPos | QOut( cFile ) } )
 *        QOut( "File was successfully created" )
 *     ENDIF
 *
 *     aFiles := { "test.prg", "C:\windows\win.ini" }
 *     nLen   := Len( aFiles )
 *     aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B" , "." )
 *     GaugeDisplay( aGauge )
 *     hb_ZipFile( "test33.zip", aFiles, 9, {| cFile, nPos | GaugeUpdate( aGauge, nPos / nLen ) },, "hello" )
 *     RETURN
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
FUNCTION hb_ZipFile( cFileName,;
                     acFiles,;
                     nLevel,;
                     bUpdate,;
                     lOverwrite,;
                     cPassword,;
                     lWithPath,;
                     lWithDrive,;
                     bProgress,;
                     lFullPath,;
                     acExclude )

   LOCAL lRetVal := .T.

   LOCAL hZip
   LOCAL hHandle
   LOCAL nLen
   LOCAL cBuffer := Space( t_nReadBuffer )
   LOCAL cFileToZip
   LOCAL nPos
   LOCAL nRead
   LOCAL cName, cExt, cDrive, cPath
   LOCAL nSize

   LOCAL aExclFile
   LOCAL aProcFile
   LOCAL cFN
   LOCAL aFile
   LOCAL tmp

   DEFAULT lOverwrite TO .F.
   DEFAULT lFullPath TO .T.

   /* TODO: Implement */
   HB_SYMBOL_UNUSED( lFullPath )
   HB_SYMBOL_UNUSED( acExclude )

   IF Set( _SET_DEFEXTENSIONS )
      hb_FNameSplit( cFileName, NIL, NIL, @cExt )
      IF Empty( cExt )
         cFileName += ".zip"
      ENDIF
   ENDIF

   IF lOverwrite .AND. hb_FileExists( cFileName )
      FErase( cFileName )
   ENDIF

   IF !Empty( hZip := hb_ZipOpen( cFileName ) )

      DEFAULT acFiles TO {}
      DEFAULT acExclude TO {}
      DEFAULT lWithPath TO .F.
      DEFAULT lWithDrive TO .F.

      IF hb_isString( acFiles )
         acFiles := { acFiles }
      ENDIF
      IF hb_isString( acExclude )
         acExclude := { acExclude }
      ENDIF

      // ;

      /* NOTE: Try not to add the .zip file to itself. */
      hb_FNameSplit( cFileName, NIL, @cName, @cExt )
      aExclFile := { hb_FNameMerge( NIL, cName, cExt ) }
      FOR EACH cFN IN acExclude
         IF "?" $ cFN .OR. "*" $ cFN
            tmp := Directory( cFN )
            FOR EACH aFile IN tmp
               AAdd( aExclFile, aFile[ F_NAME ] )
            NEXT
         ELSE
            AAdd( aExclFile, cFN )
         ENDIF
      NEXT

      aProcFile := {}
      FOR EACH cFN IN acFiles
         IF "?" $ cFN .OR. "*" $ cFN
            tmp := Directory( cFN )
            FOR EACH aFile IN tmp
               IF AScan( aExclFile, {| cExclFile | hb_FileMatch( aFile[ F_NAME ], cExclFile ) } ) == 0
                  AAdd( aProcFile, aFile[ F_NAME ] )
               ENDIF
            NEXT
         ELSE
            IF AScan( aExclFile, {| cExclFile | hb_FileMatch( cFN, cExclFile ) } ) == 0
               AAdd( aProcFile, cFN )
            ENDIF
         ENDIF
      NEXT

      aExclFile := NIL

      // ;

      nPos := 1
      FOR EACH cFileToZip IN aProcFile

         IF ( hHandle := FOpen( cFileToZip, FO_READ ) ) != F_ERROR

            IF hb_isBlock( bUpdate )
               Eval( bUpdate, cFileToZip, nPos++ )
            ENDIF

            nRead := 0
            nSize := hb_FSize( cFileToZip )

            hb_FNameSplit( cFileToZip, @cPath, @cName, @cExt, @cDrive )
            hb_ZipFileCreate( hZip, hb_FNameMerge( iif( lWithPath, cPath, NIL ), cName, cExt, iif( lWithDrive, cDrive, NIL ) ),;
                NIL, NIL, NIL, NIL, NIL, nLevel, cPassword, NIL, NIL )

            DO WHILE ( nLen := FRead( hHandle, @cBuffer, Len( cBuffer ) ) ) > 0

               IF hb_isBlock( bProgress )
                  nRead += nLen
                  Eval( bProgress, nRead, nSize )
               ENDIF

               hb_ZipFileWrite( hZip, Left( cBuffer, nLen ), nLen )
            ENDDO

            hb_ZipFileClose( hZip )

            FClose( hHandle )

            /* TODO: Clear ARCHIVE bit. */
         ENDIF
      NEXT

      hb_ZipClose( hZip, t_cComment )
   ELSE
      lRetVal := .F.
   ENDIF

   RETURN lRetVal

/*
 * $DOC$
 * $FUNCNAME$
 *     hb_UnzipFile()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Unzip a compressed file
 * $SYNTAX$
 *     hb_UnzipFILE( <cFile>, <bBlock>, <lWithPath>, <cPassWord>, <cPath>,
 *                   <cFile> | <aFile>, <pFileProgress> ) --> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file to extract
 *
 *     <bBlock>  Code block to execute while extracting
 *
 *     <lWithPath> Toggle to create directory if needed
 *
 *     <cPassWord> Password to use to extract files
 *
 *     <cPath>    Path to extract the files to - mandatory
 *
 *     <cFile> | <aFiles> A File or Array of files to extract - mandatory
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if all file was successfully restored, otherwise .F.
 * $DESCRIPTION$
 *     This function restores all files contained inside the <cFile>.
 *     If the extension is omitted, .zip will be assumed. If a file already
 *     exists, it will be overwritten.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     The <cPath> is a mandatory parameter. Set to ".\" to extract to the
 *     current directory
 *
 *     If <cFile> or <aFiles> are not provided, no files will be extracted!
 *     Make sure you provide the file or files you want extracted
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {| nPos, nTotal | GaugeUpdate( aGauge1, nPos / nTotal ) }
 * $EXAMPLES$
 *     PROCEDURE Main()
 *
 *     aExtract := hb_GetFilesInZip( "test.zip" )  // extract all files in zip
 *     IF hb_UnzipFile( "test.zip",,,, ".\", aExtract )
 *        QOut( "File was successfully extracted" )
 *     ENDIF
 *
 *     aExtract := hb_GetFilesInZip( "test2.zip" )  // extract all files in zip
 *     IF hb_UnzipFile( "test2.zip", {| cFile | QOut( cFile ) },,, ".\", aExtract )
 *        QOut( "File was successfully extracted" )
 *     ENDIF
 *     RETURN
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */
FUNCTION hb_UnzipFile( cFileName, bUpdate, lWithPath, cPassword, cPath, acFiles, bProgress )

   LOCAL lRetVal := .T.

   LOCAL hUnzip
   LOCAL nErr
   LOCAL nPos
   LOCAL cZipName
   LOCAL cExt
   LOCAL lExtract

   DEFAULT lWithPath TO .F.

   /* TODO: Implement. */
   HB_SYMBOL_UNUSED( lWithPath )
   HB_SYMBOL_UNUSED( bProgress )

   IF Empty( cPassword )
      cPassword := NIL
   ENDIF

   IF Set( _SET_DEFEXTENSIONS )
      hb_FNameSplit( cFileName, NIL, NIL, @cExt )
      IF Empty( cExt )
         cFileName += ".zip"
      ENDIF
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )

      IF hb_isNumeric( acFiles ) .OR. ;
         hb_isString( acFiles )
         acFiles := { acFiles }
      ENDIF

      IF Empty( cPath )
         hb_FNameSplit( cFileName, @cPath )
      ENDIF

      nPos := 0
      nErr := hb_UnzipFileFirst( hUnzip )
      DO WHILE nErr == 0

         nPos++

         IF hb_UnzipFileInfo( hUnzip, @cZipName ) == 0
            /* NOTE: As opposed to original hbziparch we don't do a second match without path. */
            IF !Empty( acFiles )
               IF AScan( acFiles, nPos ) > 0 .OR. ;
                  AScan( acFiles, {| cMask | hb_FileMatch( cZipName, cMask ) } ) > 0
                  lExtract := .T.
               ELSE
                  lExtract := .F.
               ENDIF
            ELSE
               lExtract := .T.
            ENDIF

            IF lExtract
               IF hb_isBlock( bUpdate )
                  Eval( bUpdate, cZipName, nPos )
               ENDIF
               hb_UnzipExtractCurrentFile( hUnzip, cPath + cZipName, cPassword )
            ENDIF
         ENDIF

         nErr := hb_UnzipFileNext( hUnzip )
      ENDDO

      hb_UnzipClose( hUnzip )
   ELSE
      lRetVal := .F.
   ENDIF

   RETURN lRetVal

FUNCTION hb_UnzipFileIndex( ... )
   RETURN hb_UnzipFile( ... )

FUNCTION hb_UnzipAllFile( ... )
   RETURN hb_UnzipFile( ... )

/* $DOC$
 * $FUNCNAME$
 *     hb_ZipDeleteFiles()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Delete files from an zip archive
 * $SYNTAX$
 *     hb_ZipDeleteFiles( <cFile>, <cFiletoDelete> | <aFiles> | <nFilePos> ) --> <lDeleted>
 * $ARGUMENTS$
 *     <cFile>  The name of the zip files from where the files will be deleted
 *
 *     <cFiletoDelete> An File to be removed
 *        _or_
 *     <aFiles>    An Array of Files to be removed
 *        _or_
 *     <nFilePos> The Position of the file to be removed
 * $RETURNS$
 *     <lDeleted> If the files are deleted, it will return .T.; otherwise
 *     it will return .F. in the following cases: Spanned Archives; the file(s)
 *     could not be found in the zip file.
 * $DESCRIPTION$
 *     This  function removes files from an Zip archive.
 * $EXAMPLES$
 *     ? "has the file zipnew.i been deleted ", iif( hb_ZipDeleteFiles( "\test23.zip", "zipnew.i" ), "Yes", "No" )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparc
 * $END$
 */

/* NOTE: Numeric file positions are not supported. */
FUNCTION hb_ZipDeleteFiles( cFileName, acFiles )

   LOCAL lRetVal := .T.
   LOCAL cFileToProc
   LOCAL cExt

   IF Set( _SET_DEFEXTENSIONS )
      hb_FNameSplit( cFileName, NIL, NIL, @cExt )
      IF Empty( cExt )
         cFileName += ".zip"
      ENDIF
   ENDIF

   IF hb_isString( acFiles )
      acFiles := { acFiles }
   ENDIF

   FOR EACH cFileToProc IN acFiles
      lRetVal := lRetVal .AND. hb_ZipDeleteFile( cFileName, cFileToProc )
   NEXT

   RETURN lRetVal
