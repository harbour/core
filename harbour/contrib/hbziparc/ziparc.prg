/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ZipArchive interface compatibility implementation.
 *
 * Copyright 2008 Viktor Szakats (harbour syenar.net)
 * Copyright 2008 Toninho (toninhofwi yahoo.com.br)
 * Copyright 2000-2001 Luiz Rafael Culik <culik@sl.conex.net>
 *   (original ZipArchive interface, docs)
 * www - http://harbour-project.org
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

#include "hbmzip.ch"

THREAD STATIC t_nReadBuffer := 32768
THREAD STATIC t_cComment
THREAD STATIC t_lReadOnly := .F.

PROCEDURE SetZipReadOnly( lReadOnly )

   DEFAULT lReadOnly TO .F.

   t_lReadOnly := lReadOnly

   /* TODO: Implement. */

   RETURN

PROCEDURE hb_SetZipComment( cComment )

   IF cComment == NIL .OR. ISCHARACTER( cComment )
      t_cComment := cComment
   ENDIF

   RETURN

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
   LOCAL nCRC

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

         hb_UnzipFileInfo( hUnzip, @cFileName, @dDate, @cTime, @nInternalAttr, NIL, @nMethod, @nSize, @nCompSize, @lCrypted, @cComment, @nCRC )

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
            AAdd( aFiles, { cFileName, nSize, nMethod, nCompSize, nRatio, dDate, cTime, hb_numtohex( nCRC, 8 ), nInternalAttr /* cAttr */, lCrypted, cComment } )
         ELSE
            AAdd( aFiles, cFileName )
         ENDIF

         nErr := hb_UnzipFileNext( hUnzip )
      ENDDO

      hb_UnzipClose( hUnzip )
   ENDIF

   RETURN aFiles

FUNCTION hb_ZipTestPK( cFileName )

   HB_SYMBOL_UNUSED( cFileName )

   /* NOTE: Spanning not supported. */

   RETURN 0

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

PROCEDURE hb_SetBuffer( nWriteBuffer, nExtractBuffer, nReadBuffer )

   HB_SYMBOL_UNUSED( nWriteBuffer )
   HB_SYMBOL_UNUSED( nExtractBuffer )

   IF !Empty( nReadBuffer )
      t_nReadBuffer := Min( nReadBuffer, 32768 )
   ENDIF

   RETURN

FUNCTION hb_ZipFileByTDSpan( cFileName, aFileToCompress, nLevel, bUpdate, lOverwrite, cPassword, nSpanSize, lWithPath, lWithDrive, bProgress, lFullPath, acExclude )

   HB_SYMBOL_UNUSED( nSpanSize )

   /* NOTE: Spanning not supported. */

   RETURN hb_ZipFile( cFileName, aFileToCompress, nLevel, bUpdate, lOverwrite, cPassword, lWithPath, lWithDrive, bProgress, lFullPath, acExclude )

FUNCTION hb_ZipFileByPKSpan( ... )

   /* NOTE: Spanning not supported. */

   RETURN hb_ZipFile( ... )

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
   LOCAL tTime

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

   IF !Empty( hZip := hb_ZipOpen( cFileName, iif( ! lOverwrite .AND. hb_FileExists( cFileName ), HB_ZIP_OPEN_ADDINZIP, NIL ) ) )

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
            hb_FNameSplit( cFN, NIL, @cName, @cExt )
            IF AScan( aExclFile, {| cExclFile | hb_FileMatch( hb_FNameMerge( NIL, cName, cExt ), cExclFile ) } ) == 0
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

            hb_FGetDateTime( cFileToZip, @tTime )

            hb_FNameSplit( cFileToZip, @cPath, @cName, @cExt, @cDrive )
            hb_ZipFileCreate( hZip, hb_FNameMerge( iif( lWithPath, cPath, NIL ), cName, cExt, iif( lWithDrive, cDrive, NIL ) ),;
                tTime, NIL, NIL, NIL, NIL, nLevel, cPassword, iif( Empty( cPassword ), NIL, hb_ZipFileCRC32( cFileToZip ) ), NIL )

            DO WHILE ( nLen := FRead( hHandle, @cBuffer, hb_BLen( cBuffer ) ) ) > 0

               IF hb_isBlock( bProgress )
                  nRead += nLen
                  Eval( bProgress, nRead, nSize )
               ENDIF

               hb_ZipFileWrite( hZip, cBuffer, nLen )
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

FUNCTION hb_UnzipFile( cFileName, bUpdate, lWithPath, cPassword, cPath, acFiles, bProgress )

   LOCAL lRetVal := .T.

   LOCAL hUnzip
   LOCAL nErr
   LOCAL nPos
   LOCAL cZipName
   LOCAL cExt
   LOCAL lExtract

   LOCAL hHandle
   LOCAL nSize
   LOCAL nRead
   LOCAL nLen
   LOCAL dDate
   LOCAL cTime
   LOCAL cBuffer := Space( t_nReadBuffer )

   DEFAULT lWithPath TO .F.

   IF lWithPath .AND. ! hb_DirExists( cPath ) .AND. hb_DirCreate( cPath ) != 0
      lRetVal := .F.
   ENDIF

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

      cPath := hb_DirSepAdd( cPath )

      nPos := 0
      nErr := hb_UnzipFileFirst( hUnzip )
      DO WHILE nErr == 0

         nPos++

         IF hb_UnzipFileInfo( hUnzip, @cZipName, @dDate, @cTime, , , , @nSize ) == 0

            /* NOTE: As opposed to original hbziparch we don't do a second match without path. */
            lExtract :=  Empty( acFiles ) .OR. ;
               AScan( acFiles, nPos ) > 0 .OR. ;
               AScan( acFiles, {| cMask | hb_FileMatch( cZipName, cMask ) } ) > 0

            IF lExtract .AND. ;
               ( hHandle := FCreate( cPath + cZipName ) ) != F_ERROR

               IF hb_UnzipFileOpen( hUnzip, cPassword ) != UNZ_OK
                  lRetVal := .F.
                  EXIT
               ENDIF

               nRead := 0
               DO WHILE ( nLen := hb_UnzipFileRead( hUnzip, @cBuffer, hb_BLen( cBuffer ) ) ) > 0
                  IF hb_isBlock( bProgress )
                     nRead += nLen
                     Eval( bProgress, nRead, nSize )
                  ENDIF
                  FWrite( hHandle, cBuffer, nLen )
               ENDDO

               hb_UnzipFileClose( hUnzip )
               FClose( hHandle )

               hb_FSetDateTime( cPath + cZipName, dDate, cTime )

               IF hb_isBlock( bUpdate )
                  Eval( bUpdate, cZipName, nPos )
               ENDIF
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
