/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 file functions:
 *    FILECOPY(), FILECOPEN(), FILECCLOSE(), FILEAPPEND()
 *
 *    Program..: fcopy.prg
 *    Author...: Frederic J. Bell
 *    Dated....: Jun,17 94
 *    Revised..: Sep,20 94
 *    Purpose..: Replaces the following ca-tools functions which generate GPF's
 *               FileCopy(), FileCOpen() & FileAppend()!
 *    Relies on: Clipper (can you believe it!)
 *    Compile..: /n /m /w /[/p /b /l] /es2
 *    Notes....:
 *    No copyright - released into the public domain NSA.
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 *    added FILECDATI() and rewritten above functions for CT3 compatibility
 *    and some problems fixing, 
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
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
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


#include "fileio.ch"
#include "common.ch"
#define  F_BLOCK   512

STATIC s_hSrcFile := -1
STATIC s_lSetDaTi := .T.
STATIC s_fileDate, s_fileTime

/*
 * FileCopy()
 * This is a replacement for the CA-tools III function of the
 * same name that causes GPF's.
 */
FUNCTION FILECOPY( cSource, cDest, lMode )
   LOCAL hDstFile
   LOCAL cBuffer := SPACE( F_BLOCK )
   LOCAL lDone := .F.
   LOCAL nSrcBytes, nDstBytes, nTotBytes := 0

   IF !ISLOGICAL( lMode )
      lMode := .F.
   ENDIF
   IF s_hSrcFile != -1
      FCLOSE( s_hSrcFile )
   ENDIF
   s_hSrcFile := FOPEN( cSource, FO_READ )
   IF s_hSrcFile != -1
      hDstFile := FCREATE( cDest )
      IF hDstFile != -1
         DO WHILE !lDone
            nSrcBytes := FREAD( s_hSrcFile, @cBuffer, F_BLOCK )
            IF nSrcBytes == 0
               lDone := .T.
               EXIT
            ENDIF
            nDstBytes := FWRITE( hDstFile, cBuffer, nSrcBytes )
            IF nDstBytes > 0
               nTotBytes += nDstBytes
            ENDIF
            IF nDstBytes < nSrcBytes
               EXIT
            ENDIF
         ENDDO
         FCLOSE( hDstFile )
         IF lDone .OR. !lMode
            FCLOSE( s_hSrcFile )
            s_hSrcFile := -1
         ENDIF
         s_fileDate := FILEDATE( cSource )
         s_fileTime := FILETIME( cSource )
         IF s_lSetDaTi
            SETFDATI( cDest, s_fileDate, s_fileTime )
         ENDIF
      ELSE
         FCLOSE( s_hSrcFile )
         s_hSrcFile := -1
      ENDIF
   ENDIF
RETURN nTotBytes


FUNCTION FILECOPEN()
RETURN s_hSrcFile != -1


FUNCTION FILECDATI( lNewMode )
   LOCAL lOldMode := s_lSetDaTi
   IF ISLOGICAL( lNewMode )
      s_lSetDaTi := lNewMode
   ENDIF
RETURN lOldMode


FUNCTION FILECCONT( cDest )
   LOCAL hDstFile
   LOCAL cBuffer := SPACE( F_BLOCK )
   LOCAL lDone := .F.
   LOCAL nSrcBytes, nDstBytes, nTotBytes := 0

   IF s_hSrcFile != -1
      hDstFile := FCREATE( cDest )
      IF hDstFile != -1
         DO WHILE !lDone
            nSrcBytes := FREAD( s_hSrcFile, @cBuffer, F_BLOCK )
            IF nSrcBytes == 0
               lDone := 0
               EXIT
            ENDIF
            nDstBytes := FWRITE( hDstFile, cBuffer, nSrcBytes )
            IF nDstBytes > 0
               nTotBytes += nDstBytes
            ENDIF
            IF nDstBytes < nSrcBytes
               EXIT
            ENDIF
         ENDDO
         FCLOSE( hDstFile )
         IF lDone
            FCLOSE( s_hSrcFile )
            s_hSrcFile := -1
         ENDIF
         IF s_lSetDaTi
            SETFDATI( cDest, s_fileDate, s_fileTime )
         ENDIF
      ENDIF
   ENDIF
RETURN nTotBytes


FUNCTION FILECCLOSE()
   IF s_hSrcFile != -1
      FCLOSE( s_hSrcFile )
      s_hSrcFile := -1
      RETURN .T.
   ENDIF
RETURN .F.


FUNCTION FILEAPPEND( cSrc, cDest )
   LOCAL cBuffer := Space( F_BLOCK )
   LOCAL hSrcFile, hDstFile
   LOCAL nSrcBytes, nDstBytes, nTotBytes := 0

   hSrcFile  := FOPEN( cSrc, FO_READ )
   IF hSrcFile != -1
      IF !FILE( cDest )
         hDstFile := FCREATE( cDest )
      ELSE
         hDstFile := FOPEN( cDest, FO_WRITE )
         FSEEK( hDstFile, 0, FS_END )
      ENDIF

      IF hDstFile != -1
         DO WHILE .T.
            nSrcBytes := FREAD( hSrcFile, @cBuffer, F_BLOCK )
            IF nSrcBytes == 0
               EXIT
            ENDIF
            nDstBytes := FWRITE( hDstFile, cBuffer, nSrcBytes )
            IF nDstBytes < nSrcBytes
               EXIT
            ENDIF
            nTotBytes += nDstBytes
         ENDDO
         FCLOSE( hDstFile )
      ENDIF
      FCLOSE( hSrcFile )
   ENDIF
RETURN nTotBytes
