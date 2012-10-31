/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HTMLLIB Frame Class
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://harbour-project.org
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
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "cgi.ch"


CREATE CLASS THtmlFrameSet

   VAR nH
   VAR FName
   VAR cStr INIT ""

   VAR TITLE INIT "FrameSet01"

   METHOD New( cFName, cTitle )

   METHOD StartSet( aRows, aCols, onLoad, onUnload )

   METHOD EndSet()

   METHOD END ()

   METHOD Frame( cName, cURL, lBorder, lResize, lScrolling, ;
      marginwidth, marginheight, cTarget, cScrolling )

ENDCLASS


METHOD New( cFName, cTitle ) CLASS THtmlFrameSet

   LOCAL cStr

   __defaultNIL( @cTitle, "" )

   ::FName := cFName
   ::Title := cTitle

   IF HB_ISSTRING( ::FName )
      cStr := ""
      ::nH := FCreate( ::FName )
   ELSE
      cStr := "Content-Type: text/html" + CRLF() + CRLF()
      ::nH := STD_OUT
   ENDIF

   cStr += "<html>" + CRLF() + ;
      " <head>" + CRLF() + ;
      "  <title>" + ::Title + "</title>" + CRLF() + ;
      " </head>" + CRLF()

   ::cStr += cStr

   RETURN Self

METHOD StartSet( aRows, aCols, onLoad, onUnload ) CLASS THtmlFrameSet

   LOCAL cStr
   LOCAL cItem

   cStr := CRLF() + " <frameset "

   IF HB_ISARRAY( aRows ) .AND. !Empty( aRows )

      cStr += ' rows="'

      FOR EACH cItem in aRows
         IF cItem:__enumIndex() > 1
            cStr += ","
         ENDIF
         cStr += cItem
      NEXT

      cStr += '"'
   ENDIF

   IF HB_ISARRAY( aCols ) .AND. !Empty( aCols )

      cStr += ' cols="'

      FOR EACH cItem IN aCols
         IF cItem:__enumIndex() > 1
            cStr += ","
         ENDIF
         cStr += cItem
      NEXT

      cStr += '"'
   ENDIF

   IF HB_ISSTRING( onLoad )
      cStr += Space( 7 ) + ' onLoad="' + onLoad + '"'
   ENDIF

   IF HB_ISSTRING( onUnLoad )
      cStr += Space( 5 ) + ' onUnLoad="' + onUnLoad + '"'
   ENDIF

   cStr += " >" + CRLF()

   ::cStr += cStr

   RETURN Self


METHOD Endset() CLASS THtmlFrameSet

   ::cStr += " </frameset>" + CRLF()

   RETURN Self


METHOD END () CLASS THtmlFrameSet

   ::cStr += "</html>" + CRLF()

   FWrite( ::nH, ::cStr )

   IF ::FName != NIL
      FClose( ::nH )
   ENDIF

   RETURN Self


METHOD Frame( cName, cURL, lBorder, lResize, lScrolling, ;
      marginwidth, marginheight, cTarget, cScrolling ) CLASS THtmlFrameSet

   LOCAL cStr

   __defaultNIL( @lBorder, .T. )
   __defaultNIL( @lResize, .T. )
   __defaultNIL( @lScrolling, .F. )
   __defaultNIL( @cScrolling, "AUTO" )
   __defaultNIL( @cTarget, "_self" )

   cStr := "  <frame "

   IF HB_ISSTRING( cName )
      cStr += ' name="' + cName + '"'
   ENDIF

   IF HB_ISSTRING( cUrl )
      cStr += ' src="' + cURL + '"'
   ENDIF

   IF HB_ISSTRING( cTarget )
      cStr += ' target="' + cTarget + '"'
   ENDIF

   IF ! lBorder
      cStr += ' frameborder="0"'
   ELSE
      cStr += ' frameborder="1"'
   ENDIF

   IF ! lResize
      cStr += " noresize"
   ENDIF

   IF HB_ISSTRING( cScrolling )
      cStr += ' scrolling="' + cScrolling + '"'
   ELSE
      IF lScrolling != NIL
         cStr += ' scrolling=' + iif( lScrolling, '"yes"', '"no"' )
      ELSE
         cStr += ' scrolling="auto"'
      ENDIF
   ENDIF

   IF HB_ISNUMERIC( marginwidth )
      cStr += " marginwidth= " + hb_ntos( marginwidth )
   ENDIF

   IF HB_ISNUMERIC( marginheight )
      cStr += " marginheight= " + hb_ntos( marginheight )
   ENDIF

   cStr += ">" + CRLF()

   ::cStr += cStr

   RETURN Self
