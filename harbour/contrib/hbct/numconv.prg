/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - NTOC()
 *                                              - CTON()
 *                                              - BITTOC()
 *                                              - CTOBIT()
 *
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
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

#include "common.ch"

#define WORLD   "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

FUNCTION NTOC( xNum, nBase, nLenght, cPad )
   LOCAL cNum

   IF ! ISCHARACTER( cPad )
      cPad := " "
   ENDIF
   IF ! ISNUMBER( nBase )
      nBase := 10
   ENDIF

   IF ISCHARACTER( xNum )
      xNum := Upper( AllTrim( xNum ) )
      xNum := CTON( xNum, 16 )
   ENDIF

   IF nBase > 36 .OR. nBase < 2
      RETURN ""
   ENDIF

   IF xNum < 0
      xNum += 4294967296
   ENDIF
   cNum := B10TOBN( xNum, @nBase )

   IF ISNUMBER( nLenght )
      IF Len( cNum ) > nLenght
         cNum := Replicate( "*", nLenght )
      ELSEIF ISCHARACTER( cPad ) .AND. Len( cNum ) < nLenght
         cNum := Replicate( cPad, nLenght - Len( cNum ) ) + cNum
      ENDIF
   ENDIF

   RETURN cNum

FUNCTION CTON( xNum, nBase, lMode )
   LOCAL i
   LOCAL nNum := 0
   LOCAL nPos

   IF ! ISLOGICAL( lMode )
      lMode := .F.
   ENDIF
   IF ! ISNUMBER( nBase )
      nBase := 10
   ENDIF

   IF ISCHARACTER( xNum ) .AND. nBase >= 2 .AND. nBase <= 36

      xNum := Upper( AllTrim( xNum) )

      FOR i := 1 TO Len( xNum )
         nPos := At( SubStr( xNum, i, 1 ), WORLD )
         IF nPos == 0 .OR. nPos > nBase
            EXIT
         ELSE
            nNum := nNum * nBase + ( nPos - 1 )
         ENDIF
      NEXT

      IF lMode .AND. nNum > 32767
         nNum := nNum - 65536
      ENDIF

   ENDIF

   RETURN nNum

STATIC FUNCTION B10TOBN( nNum, nBase )
   LOCAL nInt

   IF nNum > 0
      nInt := Int( nNum / nBase)
      RETURN iif( nInt == 0, "", B10TOBN( nInt, @nBase ) ) +;
             SubStr( WORLD, ( nNum % nBase ) + 1, 1 )
   ELSEIF nNum == 0
      RETURN "0"
   ENDIF

   RETURN ""

FUNCTION BITTOC( nInteger, cBitPattern, lMode )
   LOCAL cBinary
   LOCAL nI
   LOCAL cString := ""

   IF ! ISLOGICAL( lMode )
      lMode := .F.
   ENDIF

   cBitPattern := Right( cBitPattern, 16 )
   cBinary := NTOC( nInteger, 2, 16 )

   FOR nI := 1 TO 16
      IF SubStr( cBinary, -nI, 1 ) == "1"
         cString := SubStr( cBitPattern, -nI, 1 ) + cString
      ELSEIF lMode
         cString := " " + cString
      ENDIF
   NEXT

   RETURN Right( cString, Len( cBitPattern ) )

FUNCTION CTOBIT( cCharString, cBitPattern )
   LOCAL nI, cString := ""

   cCharString := Right( cCharString, 16 )
   cBitPattern := Right( cBitPattern, 16 )

   FOR nI := 1 TO Len( cBitPattern )
      cString := iif( At( SubStr( cBitPattern, -nI, 1 ), cCharString ) > 0, "1", "0" ) + cString
   NEXT

   RETURN CTON( cString, 2 )
