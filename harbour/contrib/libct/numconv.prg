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

#include "common.ch"
#define WORLD   '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

/*  $DOC$
 *  $FUNCNAME$
 *      NTOC()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NTOC (<xNumber>[, <nBase>][,<nLength>][,<cPadChar>]) -> <cNumber>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is numconv.prg, library is libct.
 *  $SEEALSO$
 *      CTON()
 *  $END$
 */

FUNCTION NTOC( xNum, nBase, nLenght, cPad )
LOCAL cNum

Default cPad to "0"
Default nBase to 10

IF VALTYPE( xNum ) == "C"
   xNum = ALLTRIM( xNum )
   xNum = UPPER( xNum )
   xNum = CTON( xNum, 16 )
ENDIF
IF nBase > 36 .OR. nBase < 2
   RETURN ""
ENDIF

cNum = B10TOBN( xNum, @nBase )

IF ISNUMBER( nLenght ) .AND. ISCHARACTER( cPad ) .AND. LEN( cNum ) < nLenght
   cNum = REPLICATE( cPad, nLenght - LEN( cNum ) ) + cNum
ENDIF

RETURN cNum


/*  $DOC$
 *  $FUNCNAME$
 *      CTON()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      CTON (<xNumber>[, <nBase>][,<lMode>]) -> <nNumber>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is numconv.prg, library is libct.
 *  $SEEALSO$
 *      NTOC()
 *  $END$
 */

FUNCTION CTON( xNum, nBase, lMode )
LOCAL i, nNum:=0

Default lMode TO .F.
Default nBase TO 10

xNum = ALLTRIM(xNum)

IF nBase >= 2 .AND. nBase <= 36

   FOR i=1 TO LEN( xNum )
      nNum += (nBase ** (i-1)) * ( AT( SUBSTR( xNum, -i, 1 ), WORLD ) - 1 )
   NEXT

   IF lMode
      IF nNum > 32767
         nNum = nNum - 65536
      ENDIF
   ENDIF

ENDIF

RETURN nNum


STATIC FUNCTION B10TOBN( nNum, nBase )
IF nNum > 0
   
   RETURN B10TOBN( INT( nNum / nBase), @nBase ) +;
          SUBSTR( WORLD, ( nNum % nBase ) + 1, 1 )

ENDIF
RETURN ""


/*  $DOC$
 *  $FUNCNAME$
 *      BITTOC()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      BITTOC (<nInteger>, <cBitPattern>[,<lMode>]) -> <cBitString>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is numconv.prg, library is libct.
 *  $SEEALSO$
 *      CTOBIT()
 *  $END$
 */

FUNCTION BITTOC( nInteger, cBitPattern, lMode )

  LOCAL cBinary, nI, cString := ''

  Default lMode TO .F.


  cBitPattern := RIGHT( cBitPattern, 16 )
  cBinary = NTOC( nInteger, 2, 16 )

  FOR nI = 1 TO 16
     
     IF SUBSTR( cBinary, -nI, 1 ) == '1'

        cString = SUBSTR( cBitPattern, -nI, 1 ) + cString

     ELSEIF lMode
           
        cString = ' ' + cString

     ENDIF

  NEXT

RETURN RIGHT( cString, LEN( cBitPattern ) )

/*  $DOC$
 *  $FUNCNAME$
 *      CTOBIT()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      CTOBIT (<cBitString>, <cBitPattern>) -> <nWord>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is numconv.prg, library is libct.
 *  $SEEALSO$
 *      BITTOC()
 *  $END$
 */

FUNCTION CTOBIT( cCharString, cBitPattern )

  LOCAL nI, cString := ''

  cCharString = RIGHT( cCharString, 16 )
  cBitPattern = RIGHT( cBitPattern, 16 )

  FOR nI = 1 TO LEN( cBitPattern )

     cString = IF( AT(SUBSTR( cBitPattern, -nI, 1), cCharString) > 0, '1', '0') + cString

  NEXT

RETURN CTON( cString, 2 )

