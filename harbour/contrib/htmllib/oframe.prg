/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HTMLLIB Frame Class
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

#include "hbclass.ch"
#include "default.ch"
#include "html.ch"



CLASS Frameset
DATA nH
DATA FName
DATA Title    init "FrameSet01"


METHOD New( fName, title )
METHOD StartSet( aRows, aCols, onLoad, onUnload )
METHOD EndSet()
METHOD End()
METHOD Frame( cName, cURL, lBorder, lResize, lScrolling,;
              marginwidth, marginheight, cTarget, cScrolling )

//METHOD Debug()         INLINE __clsDebug( self ) NOSELF

ENDCLASS



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD New( cFName, cTitle ) CLASS Frameset

LOCAL cStr := ""

DEFAULT cTitle := ""

::FName := cFName
::Title := cTitle


IF ::FName == NIL
   cStr += "Content-Type: text/html"+CRLF()+CRLF()
//   cStr := ""
   ::nH :=  STD_OUT
ELSE
   cStr := ""
   ::nH := FCreate( ::FName )
ENDIF

cStr += "<HTML>"+CRLF()+;
        " <HEAD>"+CRLF()+;
        "  <TITLE>"+::Title+"</TITLE>"+CRLF()+;
        " </HEAD>"+CRLF()

FWrite( ::nH, cStr )

RETURN Self



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD StartSet( aRows, aCols, onLoad, onUnload ) CLASS Frameset
LOCAL i
LOCAL cStr := ""

cStr += CRLF()+" <FRAMESET "

IF aRows != NIL .and. VALTYPE( aRows ) == "A" .and. !EMPTY( aRows )
   cStr += ' rows="'
   FOR i=1 TO LEN( aRows )
       IF i < LEN( aRows )
          cStr += aRows[i]+","
       ELSE
          cStr += aRows[i]
       ENDIF
   NEXT
   cStr += '"'
ENDIF

IF aCols != NIL .and. VALTYPE( aCols ) == "A" .and. !EMPTY( aCols )
   cStr += ' cols="'
   FOR i=1 TO LEN( acOLS )
       IF i < LEN( acOLS )
          cStr += acOLS[i]+","
       ELSE
          cStr += acOLS[i]
       ENDIF
   NEXT
   cStr += '"'
ENDIF

IF onLoad != NIL
   cStr += SPACE(7)+' onLoad="'+onLoad+'"'
ENDIF
IF onUnLoad != NIL
   cStr += SPACE(5)+' onUnLoad="'+onUnLoad+'"'
ENDIF

cStr += " >"+CRLF()

FWrite( ::nH, cStr )

RETURN Self



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
Method Endset() CLASS Frameset
FWrite( ::nH, " </FRAMESET>"+CRLF() )
RETURN Self


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
Method End() CLASS Frameset
FWrite( ::nH, "</HTML>"+CRLF() )
IF ::FName != NIL
    FClose( ::nH )
ENDIF
RETURN Self


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD Frame( cName, cURL, lBorder, lResize, lScrolling,;
              marginwidth, marginheight, cTarget, cScrolling ) CLASS Frameset

LOCAL cStr

DEFAULT lBorder    := .T.
DEFAULT lResize    := .T.
DEFAULT lScrolling := .F.
DEFAULT cScrolling := "AUTO"
DEFAULT cTarget    := "_self"  //"_blank"

cStr := "  <FRAME "

IF cName != NIL
cStr += ' name="'+cName +'"'
ENDIF

IF cUrl != NIL
cStr += ' src="'+cURL +'"'
ENDIF

IF cTarget != NIL
cStr += ' TARGET="'+cTarget +'"'
ENDIF

IF !lBorder
cStr += ' frameborder="0"'
else
cStr += ' frameborder="1"'
ENDIF

IF !lResize
cStr += " NORESIZE"
//else
//cStr += " RESIZE"
ENDIF

IF cScrolling != NIL
      cStr += ' SCROLLING="'+cScrolling+'"'
ELSE
   IF lScrolling != NIL
      cStr += ' SCROLLING='+IF( lScrolling, '"yes"', '"no"' )
   else
      cStr += ' SCROLLING="auto"'
   ENDIF
ENDIF

IF marginwidth != NIL
cStr += " MARGINWIDTH='"+NTRIM(marginwidth)+"'"
ENDIF

IF marginheight != NIL
cStr += "MARGINHEIGHT="+NTRIM(marginwidth)
ENDIF

cStr += ">"+CRLF()

FWrite( ::nH, cStr )
RETURN Self


