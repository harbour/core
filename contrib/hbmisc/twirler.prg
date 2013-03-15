/*
 * $Id$
 */

// New libmisc twirler class
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-15 by David G. Holm <dholm@jsd-llc.com>
*/

#include "hbclass.ch"

CREATE CLASS Twirler

   VAR n_Row
   VAR n_Col
   VAR n_Index
   VAR n_Seconds
   VAR n_Smooth
   VAR c_Chars
   VAR c_Title

   METHOD new( nRow, nCol, cTitle, cChars, nSmooth )
   METHOD twirl()
   METHOD show()
   METHOD hide()

END CLASS

METHOD new( nRow, nCol, cTitle, cChars, nSmooth ) CLASS Twirler

   ::n_Row := nRow
   ::n_Col := nCol
   ::n_Smooth := nSmooth
   ::c_Chars := iif( Empty( cChars ), "|/-\", cChars )
   ::c_Title := cTitle
   IF Empty( ::c_Title )
      ::c_Title := ""
   ENDIF
   ::n_Col += Len( ::c_Title )

   RETURN Self

METHOD twirl() CLASS Twirler

   LOCAL nSeconds := Seconds()

   IF Empty( ::n_Seconds ) .OR. nSeconds - ::n_Seconds >= ::n_Smooth .OR. nSeconds < ::n_Seconds
      hb_DispOutAt( ::n_Row, ::n_Col, SubStr( ::c_Chars, ::n_Index, 1 ) )
      ::n_Index++
      if ::n_Index > Len( ::c_Chars )
         ::n_Index := 1
      ENDIF
      IF ! Empty( ::n_Seconds )
         ::n_Seconds := nSeconds
      ENDIF
   ENDIF

   RETURN Self

METHOD show() CLASS Twirler

   ::n_Index := 1
   IF ! Empty( ::n_Smooth )
      ::n_Seconds := - ::n_Smooth
   ENDIF
   hb_DispOutAt( ::n_Row, ::n_Col - Len( ::c_Title ), ::c_Title )

   RETURN Self

METHOD hide() CLASS Twirler

   hb_DispOutAt( ::n_Row, ::n_Col - Len( ::c_Title ), Space( Len( ::c_Title ) + 1 ) )

   RETURN Self
