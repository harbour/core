/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 file functions:
 *    FILECOPY(), FILECOPEN(), FILECCLOSE(), FILEAPPEND()
 *
 *    Author...: Frederic J. Bell
 *    Dated....: Jun,17 94
 *    Revised..: Sep,20 94
 *    Purpose..: Replaces the following CA-T*ols functions which generate GPF's
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
 * www - http://harbour-project.org
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

#define F_BLOCK   512

THREAD STATIC t_hSrcFile := F_ERROR
THREAD STATIC t_lSetDaTi := .T.
THREAD STATIC t_fileDate
THREAD STATIC t_fileTime

/*
 * FileCopy()
 * This is a replacement for the CA-T*ols III function of the
 * same name that causes GPF's.
 */

FUNCTION FileCopy( cSource, cDest, lMode )

   LOCAL hDstFile
   LOCAL cBuffer := Space( F_BLOCK )
   LOCAL lDone := .F.
   LOCAL nSrcBytes, nDstBytes, nTotBytes := 0

   IF ! HB_ISLOGICAL( lMode )
      lMode := .F.
   ENDIF
   IF t_hSrcFile != F_ERROR
      FClose( t_hSrcFile )
   ENDIF
   t_hSrcFile := FOpen( cSource, FO_READ )
   IF t_hSrcFile != F_ERROR
      hDstFile := FCreate( cDest )
      IF hDstFile != F_ERROR
         DO WHILE ! lDone
            nSrcBytes := FRead( t_hSrcFile, @cBuffer, F_BLOCK )
            IF nSrcBytes == 0
               lDone := .T.
               EXIT
            ENDIF
            nDstBytes := FWrite( hDstFile, cBuffer, nSrcBytes )
            IF nDstBytes > 0
               nTotBytes += nDstBytes
            ENDIF
            IF nDstBytes < nSrcBytes
               EXIT
            ENDIF
         ENDDO
         FClose( hDstFile )
         IF lDone .OR. ! lMode
            FClose( t_hSrcFile )
            t_hSrcFile := F_ERROR
         ENDIF
         t_fileDate := FileDate( cSource )
         t_fileTime := FileTime( cSource )
         IF t_lSetDaTi
            SetFDaTi( cDest, t_fileDate, t_fileTime )
         ENDIF
      ELSE
         FClose( t_hSrcFile )
         t_hSrcFile := F_ERROR
      ENDIF
   ENDIF

   RETURN nTotBytes

FUNCTION FileCOpen()

   RETURN t_hSrcFile != F_ERROR

FUNCTION FileCDaTi( lNewMode )

   LOCAL lOldMode := t_lSetDaTi

   IF HB_ISLOGICAL( lNewMode )
      t_lSetDaTi := lNewMode
   ENDIF

   RETURN lOldMode

FUNCTION FileCCont( cDest )

   LOCAL hDstFile
   LOCAL cBuffer := Space( F_BLOCK )
   LOCAL lDone := .F.
   LOCAL nSrcBytes, nDstBytes, nTotBytes := 0

   IF t_hSrcFile != F_ERROR
      hDstFile := FCreate( cDest )
      IF hDstFile != F_ERROR
         DO WHILE ! lDone
            nSrcBytes := FRead( t_hSrcFile, @cBuffer, F_BLOCK )
            IF nSrcBytes == 0
               lDone := 0
               EXIT
            ENDIF
            nDstBytes := FWrite( hDstFile, cBuffer, nSrcBytes )
            IF nDstBytes > 0
               nTotBytes += nDstBytes
            ENDIF
            IF nDstBytes < nSrcBytes
               EXIT
            ENDIF
         ENDDO
         FClose( hDstFile )
         IF lDone
            FClose( t_hSrcFile )
            t_hSrcFile := F_ERROR
         ENDIF
         IF t_lSetDaTi
            SetFDaTi( cDest, t_fileDate, t_fileTime )
         ENDIF
      ENDIF
   ENDIF

   RETURN nTotBytes

FUNCTION FileCCLose()

   IF t_hSrcFile != F_ERROR
      FClose( t_hSrcFile )
      t_hSrcFile := F_ERROR
      RETURN .T.
   ENDIF

   RETURN .F.

FUNCTION FileAppend( cSrc, cDest )

   LOCAL cBuffer := Space( F_BLOCK )
   LOCAL hSrcFile, hDstFile
   LOCAL nSrcBytes, nDstBytes, nTotBytes := 0

   hSrcFile := FOpen( cSrc, FO_READ )
   IF hSrcFile != F_ERROR
      IF ! hb_FileExists( cDest )
         hDstFile := FCreate( cDest )
      ELSE
         hDstFile := FOpen( cDest, FO_WRITE )
         FSeek( hDstFile, 0, FS_END )
      ENDIF

      IF hDstFile != F_ERROR
         DO WHILE .T.
            nSrcBytes := FRead( hSrcFile, @cBuffer, F_BLOCK )
            IF nSrcBytes == 0
               EXIT
            ENDIF
            nDstBytes := FWrite( hDstFile, cBuffer, nSrcBytes )
            IF nDstBytes < nSrcBytes
               EXIT
            ENDIF
            nTotBytes += nDstBytes
         ENDDO
         FClose( hDstFile )
      ENDIF
      FClose( hSrcFile )
   ENDIF

   RETURN nTotBytes
