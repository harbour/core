/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - NTOC()
 *                                              - CTON()
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

IF VALTYPE( xNum ) == "C"
   xNum = ALLTRIM( xNum )
   xNum = UPPER( xNum )
   xNum = CTON( xNum, 16 )
ENDIF
IF nBase > 36 .OR. nBase < 2
   RETURN ""
ENDIF

cNum = B10TOBN( xNum, @nBase )

IF ISNUMBER( nLenght ) .AND. LEN( cNum ) < nLenght
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
LOCAL i, nNum:=0, cWorld := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

Default lMode TO .F.

xNum = ALLTRIM(xNum)

FOR i=LEN( xNum ) TO 1 STEP -1
   nNum += (nBase ** i) * AT(SUBSTR(xNum,i,1),cWorld)
NEXT

IF lMode
  IF nNum > 32767
    nNum = nNum - 65536
  ENDIF
ENDIF

RETURN nNum


STATIC FUNCTION B10TOBN( xNum, nBase )
LOCAL nParcial, cWorld := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

IF xNum > 0
   
   nParcial = xNum % nBase
   RETURN B10TOBN( INT(xNum/nBase), @nBase ) + SUBSTR( cWorld, nParcial, 1 )

ENDIF
RETURN ""

