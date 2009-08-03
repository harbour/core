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
#include "common.ch"
#include "cgi.ch"


CLASS THtmlFrameSet

   DATA nH
   DATA FName
   Data cStr INIT ""

   DATA TITLE INIT "FrameSet01"

   METHOD New( fName, title )

   METHOD StartSet( aRows, aCols, onLoad, onUnload )

   METHOD EndSet()

   METHOD END ()

   METHOD Frame( cName, cURL, lBorder, lResize, lScrolling, ;
   marginwidth, marginheight, cTarget, cScrolling )

ENDCLASS

   
METHOD New( cFName, cTitle ) CLASS THtmlFrameSet

   LOCAL cStr := ""

   DEFAULT cTitle TO ""

   ::FName := cFName
   ::Title := cTitle

   IF ::FName == NIL
      cStr += "Content-Type: text/html" + CRLF() + CRLF()
      //   cStr := ""
      ::nH := STD_OUT
   ELSE
      cStr := ""
      ::nH := Fcreate( ::FName )
   ENDIF

   cStr += "<HTML>" + CRLF() + ;
                             " <HEAD>" + CRLF() + ;
                             "  <TITLE>" + ::Title + "</TITLE>" + CRLF() + ;
                             " </HEAD>" + CRLF()

   ::cStr +=  cStr 

RETURN Self

METHOD StartSet( aRows, aCols, onLoad, onUnload ) CLASS THtmlFrameSet

   LOCAL i
   LOCAL cStr := ""
   LOCAL cItem

   cStr += CRLF() + " <FRAMESET "

   IF aRows != NIL .and. Valtype( aRows ) == "A" .and. !Empty( aRows )
      cStr += ' rows="'

      FOR EACH cItem in aRows
         i:=HB_ENUMINDEX()

         IF i < Len( aRows )
            cStr += cItem + ","
         ELSE
            cStr += cItem
         ENDIF

      NEXT
      cStr += '"'
   ENDIF

   IF aCols != NIL .and. Valtype( aCols ) == "A" .and. !Empty( aCols )

      cStr += ' cols="'

      FOR EACH cItem IN aCols
        i:=hb_enumindex()
         IF i < Len( aCols )
            cStr += cItem + ","
         ELSE
            cStr += cItem
         ENDIF

      NEXT

      cStr += '"'
   ENDIF

   IF onLoad != NIL
      cStr += Space( 7 ) + ' onLoad="' + onLoad + '"'
   ENDIF

   IF onUnLoad != NIL
      cStr += Space( 5 ) + ' onUnLoad="' + onUnLoad + '"'
   ENDIF

   cStr += " >" + CRLF()

   ::cStr +=  cStr 

RETURN Self


METHOD Endset() CLASS THtmlFrameSet

   ::cStr +=  " </FRAMESET>" + CRLF() 

RETURN Self


METHOD END () CLASS THtmlFrameSet

   ::cStr +=  "</HTML>" + CRLF() 
   FWrite( ::nH, ::cStr )

   IF ::FName != NIL
      Fclose( ::nH )
   ENDIF

RETURN Self


METHOD Frame( cName, cURL, lBorder, lResize, lScrolling, ;
                 marginwidth, marginheight, cTarget, cScrolling ) CLASS THtmlFrameSet

   LOCAL cStr

   DEFAULT lBorder TO .T.
   DEFAULT lResize TO .T.
   DEFAULT lScrolling TO .F.
   DEFAULT cScrolling TO "AUTO"
   DEFAULT cTarget TO "_self"  

   cStr := "  <FRAME "

   IF cName != NIL
      cStr += ' name="' + cName + '"'
   ENDIF

   IF cUrl != NIL
      cStr += ' src="' + cURL + '"'
   ENDIF

   IF cTarget != NIL
      cStr += ' TARGET="' + cTarget + '"'
   ENDIF

   IF !lBorder
      cStr += ' frameborder="0"'
   ELSE
      cStr += ' frameborder="1"'
   ENDIF

   IF !lResize
      cStr += " NORESIZE"
   ENDIF

   IF cScrolling != NIL
      cStr += ' SCROLLING="' + cScrolling + '"'
   ELSE

      IF lScrolling != NIL
         cStr += ' SCROLLING=' + IF( lScrolling, '"yes"', '"no"' )
      ELSE
         cStr += ' SCROLLING="auto"'
      ENDIF

   ENDIF

   IF marginwidth != NIL
      cStr += " MARGINWIDTH= " + NTRIM( marginwidth ) 
   ENDIF

   IF marginheight != NIL
      cStr += " MARGINHEIGHT= " + NTRIM( marginheight )
   ENDIF

   cStr += ">" + CRLF()

   ::cStr +=  cStr 

   RETURN Self
