/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-04-03 by David G. Holm <dholm@jsd-llc.com>
   New libmisc twirler class
 */

#include "hbclass.ch"

CREATE CLASS Twirler

   METHOD new( nRow, nCol, cTitle, cChars, nSmooth )
   METHOD twirl()
   METHOD show()
   METHOD hide()

   PROTECTED:

   VAR nRow
   VAR nCol
   VAR nIndex
   VAR nSeconds
   VAR nSmooth
   VAR cChars
   VAR cTitle

ENDCLASS

METHOD new( nRow, nCol, cTitle, cChars, nSmooth ) CLASS Twirler

   ::nRow := nRow
   ::nCol := nCol
   ::nSmooth := hb_defaultValue( nSmooth, 0 )
   ::cChars := hb_defaultValue( cChars, "|/-\" )
   ::cTitle := hb_defaultValue( cTitle, "" )
   ::nCol += Len( ::cTitle )

   RETURN Self

METHOD twirl() CLASS Twirler

   LOCAL nSeconds := Seconds()

   IF Empty( ::nSeconds ) .OR. nSeconds - ::nSeconds >= ::nSmooth .OR. nSeconds < ::nSeconds
      hb_DispOutAt( ::nRow, ::nCol, SubStr( ::cChars, ::nIndex, 1 ) )
      ::nIndex++
      if ::nIndex > Len( ::cChars )
         ::nIndex := 1
      ENDIF
      IF ! Empty( ::nSeconds )
         ::nSeconds := nSeconds
      ENDIF
   ENDIF

   RETURN Self

METHOD show() CLASS Twirler

   ::nIndex := 1
   IF ! Empty( ::nSmooth )
      ::nSeconds := - ::nSmooth
   ENDIF
   hb_DispOutAt( ::nRow, ::nCol - Len( ::cTitle ), ::cTitle )

   RETURN Self

METHOD hide() CLASS Twirler

   hb_DispOutAt( ::nRow, ::nCol - Len( ::cTitle ), Space( Len( ::cTitle ) + 1 ) )

   RETURN Self
