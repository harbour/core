/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Misc Suport Functions for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include "html.ch"
#include "default.ch"


/****
*
*     backButton()
*
*
*
*/

PROC BackButton( cImage, oHtm )

DEFAULT cImage := "back.gif"
DEFAULT oHtm   := oPage()

   IMAGE (cImage) ;
     URL "" ;
 ONCLICK "history.back()";
      OF oHtm

RETURN


/****
*
*     BackFormButton()
*
*
*
*/

PROC BackFormButton( cImage, oForm )
LOCAL oBut

DEFAULT oForm   := oForm()

IF cImage == NIL
   DEFINE BUTTON oBut ;
            NAME "BackButton" ;
           VALUE "go Back"   ;
         ONCLICK "history.back()" ;
              IN oForm
ELSE
    DEFINE IMAGE oBut ;
            NAME "BackButton";
          SOURCE (cImage) ;
         ONCLICK "history.back()";
              IN oForm
ENDIF

RETURN



/****
*
*     PutCounter()
*
*
*
*/

FUNCTION PutCounter( oHtm, nNumber, cDir, nDigits, nWidth, bgColor, nBorder )
LOCAL i
LOCAL cStr    := ""
LOCAL cLetter := ""

DEFAULT oHtm    := oPage()
DEFAULT nNumber := 0
DEFAULT cDir    := "/images/counters/"
DEFAULT nWidth  := 50
DEFAULT nDigits := LEN(ALLTRIM(STR( nNumber )))
DEFAULT nBorder := 1
DEFAULT bgColor := "black"

IF Valtype( nNumber ) == "N"
   cStr := STRZERO(nNumber, nDigits)
ENDIF

oHtm:Write("<center>")
DEFINE TABLE ;
       BORDER (nBorder) ;
       WIDTH (nWidth) ;
       COLORBG (bgColor) ;
       OF oHtm

oHtm:newTableRow()
oHtm:newTableCell("center")

FOR i=1 TO LEN( cStr )
    cLetter := SUBSTR( cStr, i, 1 )
    IMAGE cDir+cLetter+".gif" ;
    BORDER 0 ;
    OF oHtm
NEXT

oHtm:endTableCell()
oHtm:endTableRow()
oHtm:endTable()

oHtm:Write("</center>")

RETURN Nil


/****
*
*     HtmlPadL()
*
*
*
*/

FUNCTION HtmlPadL( cStr, n )
LOCAL cRet    := ""
LOCAL nStrLen, nSpaces

IF n == NIL
   RETURN cStr
ENDIF

nStrLen := LEN( cStr )
nSpaces := n - LEN( cStr )

IF n <= 0
   cRet := RIGHT(cStr, n )
ELSE
   cRet := REPLICATE( _HTML_SPACE, nSpaces )+ cStr
ENDIF

RETURN cRet


/****
*
*     HtmlPadR()
*
*
*
*/

FUNCTION HtmlPadR( cStr, n )
LOCAL cRet    := ""
LOCAL nStrLen, nSpaces

IF n == NIL
   RETURN cStr
ENDIF

nStrLen := LEN( cStr )
nSpaces := n - LEN( cStr )

IF n <= 0
   cRet := LEFT(cStr, n )
ELSE
   cRet := cStr+REPLICATE( _HTML_SPACE, nSpaces )
ENDIF

RETURN cRet


