/*
 * HTMLLIB Frame Class
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (Porting this library to Harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbclass.ch"
#include "cgi.ch"

#include "fileio.ch"

CREATE CLASS THtmlFrameSet

   VAR nH
   VAR FName
   VAR cStr INIT ""

   VAR TITLE INIT "FrameSet01"

   METHOD New( cFName, cTitle )
   METHOD StartSet( aRows, aCols, onLoad, onUnload )
   METHOD EndSet()
   METHOD End()
   METHOD Frame( cName, cURL, lBorder, lResize, lScrolling, ;
      marginwidth, marginheight, cTarget, cScrolling )

ENDCLASS

METHOD New( cFName, cTitle ) CLASS THtmlFrameSet

   LOCAL cStr

   ::FName := cFName
   ::Title := hb_defaultValue( cTitle, "" )

   IF HB_ISSTRING( ::FName )
      cStr := ""
      ::nH := FCreate( ::FName )
   ELSE
      cStr := "Content-Type: text/html" + hb_eol() + hb_eol()
      ::nH := hb_GetStdOut()
   ENDIF

   ::cStr += cStr + ;
      "<html>" + hb_eol() + ;
      " <head>" + hb_eol() + ;
      "  <title>" + ::Title + "</title>" + hb_eol() + ;
      " </head>" + hb_eol()

   RETURN Self

METHOD StartSet( aRows, aCols, onLoad, onUnload ) CLASS THtmlFrameSet

   LOCAL cStr := hb_eol() + " <frameset "
   LOCAL cItem

   IF HB_ISARRAY( aRows ) .AND. ! Empty( aRows )

      cStr += " rows=" + '"'

      FOR EACH cItem in aRows
         IF ! cItem:__enumIsFirst()
            cStr += ","
         ENDIF
         cStr += cItem
      NEXT

      cStr += '"'
   ENDIF

   IF HB_ISARRAY( aCols ) .AND. ! Empty( aCols )

      cStr += " cols=" + '"'

      FOR EACH cItem IN aCols
         IF ! cItem:__enumIsFirst()
            cStr += ","
         ENDIF
         cStr += cItem
      NEXT

      cStr += '"'
   ENDIF

   IF HB_ISSTRING( onLoad )
      cStr += Space( 7 ) + " onLoad=" + '"' + onLoad + '"'
   ENDIF

   IF HB_ISSTRING( onUnLoad )
      cStr += Space( 5 ) + " onUnLoad=" + '"' + onUnLoad + '"'
   ENDIF

   ::cStr += cStr + " >" + hb_eol()

   RETURN Self

METHOD Endset() CLASS THtmlFrameSet

   ::cStr += " </frameset>" + hb_eol()

   RETURN Self

METHOD End() CLASS THtmlFrameSet

   ::cStr += "</html>" + hb_eol()

   IF ::nH != F_ERROR
      FWrite( ::nH, ::cStr )

      IF HB_ISSTRING( ::FName )
         FClose( ::nH )
      ENDIF
   ENDIF

   RETURN Self

METHOD Frame( cName, cURL, lBorder, lResize, lScrolling, ;
      marginwidth, marginheight, cTarget, cScrolling ) CLASS THtmlFrameSet

   LOCAL cStr

   __defaultNIL( @cTarget, "_self" )

   cStr := "  <frame "

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"'
   ENDIF

   IF HB_ISSTRING( cUrl )
      cStr += " src=" + '"' + cURL + '"'
   ENDIF

   IF HB_ISSTRING( cTarget )
      cStr += " target=" + '"' + cTarget + '"'
   ENDIF

   IF hb_defaultValue( lBorder, .T. )
      cStr += " frameborder=1"
   ELSE
      cStr += " frameborder=0"
   ENDIF

   IF ! hb_defaultValue( lResize, .T. )
      cStr += " noresize"
   ENDIF

   DO CASE
   CASE HB_ISSTRING( cScrolling )
      cStr += " scrolling=" + '"' + cScrolling + '"'
   CASE HB_ISLOGICAL( lScrolling )
      cStr += " scrolling=" + '"' + iif( lScrolling, "yes", "no" ) + '"'
   OTHERWISE
      cStr += " scrolling=" + '"' + "auto" + '"'
   ENDCASE

   IF HB_ISNUMERIC( marginwidth )
      cStr += " marginwidth= " + hb_ntos( marginwidth )
   ENDIF

   IF HB_ISNUMERIC( marginheight )
      cStr += " marginheight= " + hb_ntos( marginheight )
   ENDIF

   ::cStr += cStr + ">" + hb_eol()

   RETURN Self
